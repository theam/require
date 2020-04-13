module Require.Parser
  ( Parser
  , requireDirective
  , Megaparsec.parseMaybe
  ) where

import qualified Data.Char as Char
import qualified Data.Text as Text
import Relude
import Require.Types
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec

type Parser = Megaparsec.Parsec Void Text

requireDirective :: Parser RequireDirective
requireDirective = do
  directive <- asum
    [ RequireDirective <$> requireInfo
    , AutorequireDirective <$ Megaparsec.string "autorequire"
    , moduleDirective
    ]
  Megaparsec.space
  skipLineComment
  pure directive

requireInfo :: Parser RequireInfo
requireInfo = do
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

  let defaultAlias = defaultModuleAlias module'
  return
    RequireInfo
      { riFullModuleName = module',
        riModuleAlias = maybe defaultAlias toText alias',
        riImportedTypes = maybe defaultAlias toText types'
      }

moduleDirective :: Parser RequireDirective
moduleDirective = do
  void $ Megaparsec.string "module"
  void $ Megaparsec.space1
  module' <- moduleNameParser
  -- Ignore anything further from the line.
  void $ Megaparsec.takeWhileP Nothing (const True)
  pure $ ModuleDirective module'

-- | Parses a haskell module name.
--
-- This parser is a superset of what makes a valid module name in Haskell
-- (e.g. we allow consecutive dots, lower-case first letters etc.).
moduleNameParser :: Parser ModuleName
moduleNameParser =
  fmap ModuleName $ Megaparsec.takeWhile1P Nothing $ \c ->
    Char.isAlphaNum c || c == '.' || c == '_' || c == '\''

-- | Skips a haskell line comment.
--
-- This parser never fails.
skipLineComment :: Parser ()
skipLineComment = void $ Megaparsec.optional $
  Megaparsec.string "--"
    *> (Megaparsec.space1 <|> void Megaparsec.alphaNumChar <|> Megaparsec.eof)
    *> Megaparsec.takeWhileP Nothing (const True)

-- | Extracts the module alias to be used when none is specified. This
-- corresponds to the last segment of the module's hierarchical name.
--
-- >>> defaultModuleAlias (ModuleName "Data.Text.Lazy")
-- "Lazy"
-- >>> defaultModuleAlias (ModuleName "Main")
-- "Main"
defaultModuleAlias :: ModuleName -> Text
defaultModuleAlias = Text.takeWhileEnd (/= '.') . unModuleName
