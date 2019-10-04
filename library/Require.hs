{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
module Require where

import qualified Data.Char as Char
import qualified Data.Text as Text
import Lens.Micro.Platform
import Options.Generic
import Relude
import System.Directory
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec

newtype FileName = FileName {unFileName :: Text}

newtype ModuleName = ModuleName { unModuleName :: Text }
  deriving (Eq, Show)

newtype LineNumber = LineNumber Int
  deriving (Enum)

data LineTag = LineTag !FileName !LineNumber

data FileInput = FileInput
  { fiFileName :: FileName,
    fiContent :: Text
  }

type Parser = Megaparsec.Parsec Void Text

data RequireDirective
  = ModuleDirective ModuleName
  | RequireDirective RequireInfo
  | AutorequireDirective

data RequireInfo
  = RequireInfo
      { riFullModuleName :: ModuleName,
        riModuleAlias :: Text,
        riImportedTypes :: Maybe [Text]
      }
  deriving (Show)

data CommandArguments
  = CommandArguments Text Text Text
  deriving (Generic)

instance ParseRecord CommandArguments


type LineTagPrepend = LineTag -> Text -> Text

data TransformState = TransformState
  { _tstLineTagPrepend :: !LineTagPrepend
  , _tstHostModule     :: !(Maybe ModuleName)
  , _tstAutorequired   :: !Bool
  }

makeLenses ''TransformState


initialLineTag :: FileInput -> LineTag
initialLineTag fi = LineTag (fiFileName fi) (LineNumber 1)

advanceLineTag :: LineTag -> LineTag
advanceLineTag (LineTag fn ln) = LineTag fn (succ ln)

renderLineTag :: LineTag -> Text
renderLineTag (LineTag (FileName fn) (LineNumber ln)) =
  "{-# LINE " <> show ln <> " \"" <> fn <> "\" #-}\n"


prependLineTag :: LineTagPrepend
prependLineTag = (<>) . renderLineTag

ignoreLineTag :: LineTagPrepend
ignoreLineTag = const id


findRequires :: IO (Maybe FileName)
findRequires = do
  currentDir <- getCurrentDirectory
  files <- getDirectoryContents currentDir
  let textFiles = fmap toText files
  return $ FileName . head <$> nonEmpty (filter (Text.isSuffixOf "Requires") textFiles)

readFile' :: FileName -> IO FileInput
readFile' f = FileInput f <$> readFileText (toFilePath f)

toFilePath :: FileName -> FilePath
toFilePath = toString . unFileName

fileInputLines :: FileInput -> [(LineTag, Text)]
fileInputLines fi = zip lineTags contents
  where
    lineTags = iterate advanceLineTag (initialLineTag fi)
    contents = Text.lines (fiContent fi)


requireMain :: IO ()
requireMain = do
  CommandArguments inputFile _ outputFile <- getRecord "Require Haskell preprocessor" :: IO CommandArguments
  run False Nothing (FileName inputFile) (FileName outputFile)

autorequireMain :: IO ()
autorequireMain = do
  CommandArguments inputFile _ outputFile <- getRecord "Require Haskell preprocessor" :: IO CommandArguments
  requiresFile <- findRequires
  case requiresFile of
    Nothing -> die "There is no Requires file in the system"
    Just _  -> run True requiresFile (FileName inputFile) (FileName outputFile)

run :: Bool -> Maybe FileName -> FileName -> FileName -> IO ()
run autorequire requiresFile inputFile outputFile = do
  input <- readFile' inputFile
  requires <- traverse readFile' requiresFile
  let transformed = Require.transform autorequire input requires
  writeFileText (toFilePath outputFile) transformed

transform :: Bool -> FileInput -> Maybe FileInput -> Text
transform autorequireEnabled input prepended =
  fileInputLines input
    & mapM (process (pure Nothing)) -- TODO: If the mapM overhead gets to much maybe use a streaming library.
    & flip evalState initialState
    & Text.concat       -- TODO: No need to concatenate the whole file in memory, we can create a lazy text here.
  where
    initialState = TransformState
      { _tstLineTagPrepend = prependLineTag
      , _tstHostModule     = Nothing
      , _tstAutorequired   = False
      }

    process :: State TransformState (Maybe ModuleName) -> (LineTag, Text) -> State TransformState Text
    process getHostModule (tag, line) = do
      let useTagPrep text = do
            prep <- tstLineTagPrepend <<.= ignoreLineTag
            pure $ prep tag text

      let lineWithAutorequire cond
            | autorequireEnabled && cond = do
                -- Call useTagPrep before processAutorequireContent because the
                -- latter one modifies tstLineTagPrepend which the former one uses.
                line' <- useTagPrep $ line <> "\n"
                auto  <- processAutorequireContent
                pure $ line' <> auto
            | otherwise =
                useTagPrep $ line <> "\n"

      let hasWhere =
            -- TODO: This assumes that comments have whitespace before them and
            -- that `where` has whitespace before it. But
            --    module Foo (abc)where--something else
            -- is valid in Haskell.
            Text.words line
              & takeWhile (not . ("--" `Text.isPrefixOf`))
              & elem "where"

      case Megaparsec.parseMaybe requireDirectiveParser line of
        Nothing -> do
          hasModule <- isJust <$> use tstHostModule
          lineWithAutorequire $ hasModule && hasWhere

        Just (ModuleDirective moduleName) -> do
          -- If there is already a module name, don't overwrite it.
          -- TODO: Can we emit a warning in that case?
          tstHostModule %= (<|> Just moduleName)
          lineWithAutorequire hasWhere

        Just (RequireDirective ri) ->
          -- renderImport already prepends the line tag if necessary.
          renderImport getHostModule tag ri

        Just AutorequireDirective ->
          processAutorequireContent

    processAutorequireContent = do
      alreadyAutorequired <- tstAutorequired <<.= True
      autorequireContent  <-
        if | alreadyAutorequired  -> pure ""
           | Nothing <- prepended -> pure ""
           | Just pr <- prepended -> do
               tstLineTagPrepend .= prependLineTag
               fileInputLines pr
                 & mapM (process (use tstHostModule))
                 & fmap Text.concat

      tstLineTagPrepend .= prependLineTag
      pure autorequireContent


renderImport :: MonadState TransformState m => m (Maybe ModuleName) -> LineTag -> RequireInfo -> m Text
renderImport getHostModule line RequireInfo {..} = do
    mhostModule <- getHostModule
    lineTagPrep <- use tstLineTagPrepend
    let (res, prep) =
          if mhostModule == Just riFullModuleName
             then ("", prependLineTag)
             else (typesImport <> renderLineTag line <> qualifiedImport, ignoreLineTag)
    tstLineTagPrepend .= prep
    pure $ lineTagPrep line res
  where
    types = maybe
      (Text.takeWhileEnd (/= '.') (unModuleName riFullModuleName))
      (Text.intercalate ",")
      riImportedTypes
    typesImport = Text.unwords
      [ "import"
      , unModuleName riFullModuleName
      , "(" <> types <> ")"
      ] <> "\n"
    qualifiedImport = Text.unwords
      [ "import qualified"
      , unModuleName riFullModuleName
      , "as"
      , riModuleAlias
      ] <> "\n"


requireDirectiveParser :: Parser RequireDirective
requireDirectiveParser = do
  directive <- asum
    [ RequireDirective <$> requireInfoParser
    , AutorequireDirective <$ Megaparsec.string "autorequire"
    , moduleDirectiveParser
    ]
  Megaparsec.space
  skipLineComment
  pure directive

requireInfoParser :: Parser RequireInfo
requireInfoParser = do
  void $ Megaparsec.string "require"
  void Megaparsec.space1
  module' <- moduleNameParser
  void Megaparsec.space
  alias' <- Megaparsec.try $ Megaparsec.option Nothing $ do
    void $ Megaparsec.string "as"
    void Megaparsec.space1
    Just <$> Megaparsec.some Megaparsec.alphaNumChar
  void Megaparsec.space
  types' <- Megaparsec.option Nothing $ do
    void $ Megaparsec.char '('
    t' <- Megaparsec.many (Megaparsec.alphaNumChar <|> Megaparsec.char ',' <|> Megaparsec.char ' ')
    void $ Megaparsec.char ')'
    return $ Just t'

  return
    RequireInfo
      { riFullModuleName = module',
        riModuleAlias = maybe (Text.takeWhileEnd (/= '.') $ unModuleName module') toText alias',
        riImportedTypes = (fmap Text.strip <$> Text.splitOn ",") . toText <$> types'
      }

moduleDirectiveParser :: Parser RequireDirective
moduleDirectiveParser = do
  void $ Megaparsec.string "module"
  void $ Megaparsec.space1
  module' <- moduleNameParser
  -- Ignore anything further from the line.
  void $ Megaparsec.takeWhileP Nothing (const True)
  pure $ ModuleDirective module'

moduleNameParser :: Parser ModuleName
moduleNameParser =
  -- This parser is a superset of what makes a valid module name in Haskell
  -- (e.g. we allow consecutive dots, lower-case first letters etc.)
  fmap ModuleName $ Megaparsec.takeWhile1P Nothing $ \c ->
    Char.isAlphaNum c || c == '.' || c == '_' || c == '\''

skipLineComment :: Parser ()
skipLineComment = void $ Megaparsec.optional $
  Megaparsec.string "--"
    *> (Megaparsec.space1 <|> void Megaparsec.alphaNumChar <|> Megaparsec.eof)
    *> Megaparsec.takeWhileP Nothing (const True)
