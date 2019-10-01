module Require where

import qualified Data.Text as Text
import Options.Generic
import Relude
import System.Directory
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec

newtype FileName = FileName {unFileName :: Text}

newtype ModuleName = ModuleName { unModuleName :: Text }
  deriving (Eq, Show)

newtype LineNumber = LineNumber Int

data LineTag = LineTag !FileName !LineNumber

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

findRequires :: IO (Maybe Text)
findRequires = do
  currentDir <- getCurrentDirectory
  files <- getDirectoryContents currentDir
  let textFiles = fmap toText files
  return $ head <$> nonEmpty (filter (Text.isSuffixOf "Requires") textFiles)

requireMain :: IO ()
requireMain = do
  CommandArguments inputFile _ outputFile <- getRecord "Require Haskell preprocessor" :: IO CommandArguments
  content <- readFile (toString inputFile)
  writeFile
    (toString outputFile)
    ( toString $
        Require.transform
          False
          (Require.FileName inputFile)
          ""
          (toText content)
    )

autorequireMain :: IO ()
autorequireMain = do
  CommandArguments inputFile _ outputFile <- getRecord "Require Haskell preprocessor" :: IO CommandArguments
  requiresFile <- findRequires
  case requiresFile of
    Nothing -> die "There is no Requires file in the system"
    Just x -> do
      file <- readFile $ toString x
      content <- readFile (toString inputFile)
      writeFile
        (toString outputFile)
        ( toString $
            Require.transform
              True
              (Require.FileName inputFile)
              (toText file)
              (toText content)
        )

transform :: Bool -> FileName -> Text -> Text -> Text
transform autorequireEnabled filename imports input
  | autorequireEnabled = transform' True filename imports input
  | noAutorequire = transform' False filename imports input
  | otherwise = transform' True filename imports input
  where
    noAutorequire = not (any ("autorequire" `Text.isPrefixOf`) $ lines input)

transform' :: Bool -> FileName -> Text -> Text -> Text
transform' shouldPrepend filename prepended input =
  Text.lines input
    & zip [1 ..]
    & filter (\(_, t) -> not $ "autorequire" `Text.isPrefixOf` t)
    >>= prependAfterModuleLine
    <&> (\(ln, text) -> maybe (text <> "\n") (renderImport filename (LineNumber ln)) $ Megaparsec.parseMaybe requireParser text)
    & (renderLineTag (LineTag filename (LineNumber 1)) :)
    & Text.concat
  where
    enumeratedPrepend ln
      | shouldPrepend = zip (repeat ln) (Text.lines prepended)
      | otherwise = []
    prependAfterModuleLine (ln, text)
      | ("module" `Text.isPrefixOf` text)
          && ("where" `Text.isSuffixOf`) text =
        (ln, text) : enumeratedPrepend ln
      | ("instance" `Text.isPrefixOf` text)
          && ("where" `Text.isSuffixOf`) text =
        [(ln, text)]
      | ("data" `Text.isPrefixOf` text)
          && ("where" `Text.isSuffixOf`) text =
        [(ln, text)]
      | ("class" `Text.isPrefixOf` text)
          && ("where" `Text.isSuffixOf`) text =
        [(ln, text)]
      | not ("instance" `Text.isPrefixOf` text)
          && not ("class" `Text.isPrefixOf` text)
          && not ("data" `Text.isPrefixOf` text)
          && ("where" `Text.isPrefixOf`) text =
        (ln, text) : enumeratedPrepend ln
      | otherwise = [(ln, text)]

renderLineTag :: LineTag -> Text
renderLineTag (LineTag (FileName fn) (LineNumber ln)) =
  "{-# LINE "
    <> show ln
    <> " \""
    <> fn
    <> "\" #-}\n"

renderImport :: FileName -> LineNumber -> RequireInfo -> Text
renderImport filename linenumber RequireInfo {..} =
  if unModuleName riFullModuleName `Text.isInfixOf` unFileName filename
    then ""
    else typesImport <> renderLineTag (LineTag filename linenumber) <> qualifiedImport
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
