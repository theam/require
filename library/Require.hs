module Require where

import Universum

import qualified Text.Megaparsec      as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec
import qualified Data.Text            as Text
import Options.Generic
import System.Directory

newtype FileName   = FileName { unFileName :: Text }
newtype LineNumber = LineNumber Int
type Parser        = Megaparsec.Parsec Void Text

data RequireInfo = RequireInfo
  { riFullModuleName :: Text
  , riModuleAlias    :: Text
  , riImportedTypes  :: Maybe [Text]
  } deriving Show

data CommandArguments =
  CommandArguments Text Text Text
  deriving Generic

instance ParseRecord CommandArguments

findRequires :: IO (Maybe Text)
findRequires = do
  currentDir <- getCurrentDirectory
  files <- getDirectoryContents currentDir
  let textFiles = fmap toText files
  return $ head <$> (nonEmpty $ filter (Text.isSuffixOf "Requires") textFiles)

requireMain :: IO ()
requireMain = do
  CommandArguments _ inputFile outputFile <- getRecord "Require Haskell preprocessor" :: IO CommandArguments
  content <- readFile (toString inputFile)
  writeFile
    (toString outputFile)
    ( Require.transform
        False
        (Require.FileName inputFile)
        ""
        content
    )

autorequireMain :: IO ()
autorequireMain = do
  CommandArguments _ inputFile outputFile <- getRecord "Require Haskell preprocessor" :: IO CommandArguments
  requiresFile <- findRequires
  case requiresFile of
    Nothing -> die "There is no Requires file in the system"
    Just x -> do
      file <- readFile $ toString x
      content <- readFile (toString inputFile)
      writeFile
        (toString outputFile)
        (Require.transform
          True
          (Require.FileName inputFile)
          file
          content)

transform :: Bool -> FileName -> Text -> Text -> Text
transform autorequireEnabled filename imports input
  | autorequireEnabled = transform' True filename imports input
  | noAutorequire  = transform' False filename imports input
  | otherwise      = transform' True filename imports input
 where
  noAutorequire = (length $ filter (\t -> "autorequire" `Text.isPrefixOf` t) $ lines input) == 0


transform' :: Bool -> FileName -> Text -> Text -> Text
transform' shouldPrepend filename prepended input =
  Text.lines input
  &   filter (\t -> not $ "autorequire" `Text.isPrefixOf` t)
  &   zip [1..]
  >>= prependAfterModuleLine
  <&> (\(ln, text) -> maybe (text <> "\n") (renderImport filename (LineNumber ln)) $ Megaparsec.parseMaybe requireParser text)
  &   (lineTag filename (LineNumber 1) :)
  &   Text.concat
 where
  enumeratedPrepend ln
   | shouldPrepend = zip (repeat ln) (Text.lines prepended)
   | otherwise     = []
  prependAfterModuleLine (ln, text)
   | ("module" `Text.isPrefixOf` text)
     && ("where" `Text.isSuffixOf`) text = (ln, text) : enumeratedPrepend ln
   | ("instance" `Text.isPrefixOf` text)
     && ("where" `Text.isSuffixOf`) text = [(ln, text)]
   | ("data" `Text.isPrefixOf` text)
     && ("where" `Text.isSuffixOf`) text = [(ln, text)]
   | ("class" `Text.isPrefixOf` text)
     && ("where" `Text.isSuffixOf`) text = [(ln, text)]
   | (not $ "instance" `Text.isPrefixOf` text)
     && (not $ "class" `Text.isPrefixOf` text)
     && (not $ "data" `Text.isPrefixOf` text)
     && ("where" `Text.isPrefixOf`) text = (ln, text) : enumeratedPrepend (ln)
   | otherwise                      = [(ln, text)]

lineTag :: FileName -> LineNumber -> Text
lineTag (FileName fn) (LineNumber ln) =
  "{-# LINE "
  <> show ln
  <> " \""
  <> fn
  <> "\" #-}\n"


renderImport :: FileName -> LineNumber -> RequireInfo -> Text
renderImport filename linenumber RequireInfo {..} =
  case (Text.isInfixOf riFullModuleName (unFileName filename)) of
    True  -> ""
    False -> typesImport <> lineTag filename linenumber <> qualifiedImport
 where
  types = maybe (Text.takeWhileEnd (/= '.') riFullModuleName) (Text.intercalate ",") riImportedTypes
  typesImport = "import " <> riFullModuleName <> " (" <> types <> ")\n"
  qualifiedImport = "import qualified " <> riFullModuleName <> " as " <> riModuleAlias <> "\n"


requireParser :: Parser RequireInfo
requireParser = do
  void $ Megaparsec.string "require"
  void $ Megaparsec.space1
  module' <- Megaparsec.some (Megaparsec.alphaNumChar <|> Megaparsec.punctuationChar)
  void $ Megaparsec.space

  alias'  <- Megaparsec.try $ Megaparsec.option Nothing $ do
    void $ Megaparsec.string "as"
    void $ Megaparsec.space1
    Just <$> Megaparsec.some (Megaparsec.alphaNumChar)

  void $ Megaparsec.space

  types'  <- Megaparsec.option Nothing $ do
    void $ Megaparsec.char '('
    t' <- Megaparsec.many (Megaparsec.alphaNumChar <|> Megaparsec.char ',' <|> Megaparsec.char ' ')
    void $ Megaparsec.char ')'
    return $ Just t'

  void $ Megaparsec.option Nothing $ do
    void $ Megaparsec.space
    void $ Megaparsec.some (Megaparsec.char '-')
    void $ Megaparsec.many (Megaparsec.alphaNumChar <|> Megaparsec.char ' ')
    return Nothing

  return RequireInfo
    { riFullModuleName = toText $ module'
    , riModuleAlias    = maybe (Text.takeWhileEnd (/= '.') $ toText module') toText alias'
    , riImportedTypes  = fmap Text.strip <$> Text.splitOn "," <$> toText <$> types'
    }
