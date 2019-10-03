{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Require where

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
  { _tstLineTagPrepend :: LineTagPrepend
  , _tstHostModule     :: ModuleName
  }

makeLenses ''TransformState


initialLineTag :: FileInput -> LineTag
initialLineTag fi = LineTag (fiFileName fi) (LineNumber 1)

advanceLineTag :: LineTag -> LineTag
advanceLineTag (LineTag fn ln) = LineTag fn (succ ln)

renderLineTag :: LineTag -> Text
renderLineTag (LineTag (FileName fn) (LineNumber ln)) =
  "{-# LINE " <> show ln <> " \"" <> fn <> "\" #-}\n"


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
transform autorequireEnabled input requireInput =
  transform' input $ do
    guard $ autorequireEnabled || autorequireDirective
    requireInput
 where
   autorequireDirective =
     any (\t -> "autorequire" `Text.isPrefixOf` t) $ Text.lines $ fiContent input

transform' :: FileInput -> Maybe FileInput -> Text
transform' input prepended =
  flip evalState initialState
    $ process ""
    $ fmap (second Text.stripEnd)
    $ fileInputLines input
  where
    initialState = TransformState
      { _tstHostModule = ModuleName "Main"
      , _tstLineTagPrepend = prependLineTag
      }

    prependLineTag, ignoreLineTag :: LineTagPrepend
    prependLineTag = \lt -> (renderLineTag lt <>)
    ignoreLineTag  = \_  -> id

    process :: Text -> [(LineTag, Text)] -> State TransformState Text
    process acc [] = pure acc
    process acc ((_tag, "autorequire") : remainingLines) = do
      tstLineTagPrepend .= prependLineTag
      acc' <- process acc $ foldMap fileInputLines prepended
      tstLineTagPrepend .= prependLineTag
      res  <- process acc' remainingLines
      pure res
    process acc ((tag, line) : remainingLines) = do
      tagPrep <- tstLineTagPrepend <<.= ignoreLineTag
      let acc' = tagPrep tag
            $ maybe (line <> "\n") (renderImport tag)
            $ Megaparsec.parseMaybe requireParser line
      process (acc <> acc') remainingLines

renderImport :: LineTag -> RequireInfo -> Text
renderImport line@(LineTag (FileName fn) _) RequireInfo {..} =
  if unModuleName riFullModuleName `Text.isInfixOf` fn
    then ""
    else typesImport <> renderLineTag line <> qualifiedImport
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

requireParser :: Parser RequireInfo
requireParser = do
  void $ Megaparsec.string "require"
  void Megaparsec.space1
  module' <- Megaparsec.some (Megaparsec.alphaNumChar <|> Megaparsec.punctuationChar)
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
  void $ Megaparsec.option Nothing $ do
    void Megaparsec.space
    void $ Megaparsec.some (Megaparsec.char '-')
    void $ Megaparsec.many (Megaparsec.alphaNumChar <|> Megaparsec.char ' ')
    return Nothing
  return
    RequireInfo
      { riFullModuleName = ModuleName $ toText module',
        riModuleAlias = maybe (Text.takeWhileEnd (/= '.') $ toText module') toText alias',
        riImportedTypes = (fmap Text.strip <$> Text.splitOn ",") . toText <$> types'
      }
