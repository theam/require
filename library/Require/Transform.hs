{-# LANGUAGE FlexibleContexts #-}
module Require.Transform where

import Control.Category ((>>>))
import qualified Data.Text as Text
import Relude
import qualified Require.File as File
import qualified Require.Parser as Parser
import Require.Types


type LineTagPrepend = File.LineTag -> Text -> Text

data TransformState = TransformState
  { tstLineTagPrepend :: !LineTagPrepend
  , tstHostModule     :: !(Maybe ModuleName)
  , tstAutorequire    :: !(AutorequireMode File.Input)
  }


type TransformM =
  ExceptT String
    (State TransformState)


renderLineTag :: File.LineTag -> Text
renderLineTag (File.LineTag (File.Name fn) (File.LineNumber ln)) =
  "{-# LINE " <> show ln <> " \"" <> fn <> "\" #-}\n"


prependLineTag :: LineTagPrepend
prependLineTag = (<>) . renderLineTag

ignoreLineTag :: LineTagPrepend
ignoreLineTag = const id


transform :: AutorequireMode File.Input -> File.Input -> Either String Text
transform autorequire =
  -- TODO:
  --  * if the mapM overhead is too much maybe use a streaming library
  --  * there is no need to concatenate the whole output in memory, a lazy text would be fine
  --  * maybe we should check if tstAutorequired is set after processing
  File.inputLines
    >>> mapM (process False)
    >>> runExceptT
    >>> flip evalState initialState
    >>> fmap mconcat
  where
    initialState = TransformState
      { tstLineTagPrepend = prependLineTag
      , tstHostModule     = Nothing
      , tstAutorequire    = autorequire
      }

process :: Bool -> (File.LineTag, Text) -> TransformM Text
process filterImports (tag, line) = do
  let useTagPrep text = do
        prep <- gets tstLineTagPrepend
        modify $ \s -> s { tstLineTagPrepend = ignoreLineTag }
        pure $ prep tag text

  let lineWithAutorequire isDirective autoCondition = do
        autoMode <- gets tstAutorequire
        case autoMode of
          AutorequireEnabled autoContent
            | isDirective || autoCondition -> do
                line' <- if isDirective then pure "" else useTagPrep (line <> "\n")
                auto  <- processAutorequireContent autoContent
                pure $ line' <> auto
          AutorequireOnDirective (Just autoContent)
            | isDirective -> do
                processAutorequireContent autoContent
          AutorequireOnDirective Nothing
            | isDirective ->
                -- TODO: Better error reporting.
                error "Found an `autorequire` directive but no `Requires` file was found."
          _ | isDirective -> pure ""
            | otherwise   -> useTagPrep $ line <> "\n"

  let hasWhere =
        -- TODO: This assumes that comments have whitespace before them and
        -- that `where` has whitespace before it. But
        --    module Foo (abc)where--something else
        -- is valid in Haskell.
        words line
          & takeWhile (not . ("--" `Text.isPrefixOf`))
          & elem "where"

  case Parser.parseMaybe Parser.requireDirective line of
    Nothing -> do
      hasModule <- gets $ isJust . tstHostModule
      lineWithAutorequire False $ hasModule && hasWhere

    Just (ModuleDirective moduleName) -> do
      -- If there is already a module name, don't overwrite it.
      modify $ \s -> s { tstHostModule = tstHostModule s <|> Just moduleName }
      lineWithAutorequire False hasWhere

    Just (RequireDirective ri) ->
      -- renderImport already prepends the line tag if necessary.
      renderImport filterImports tag ri

    Just AutorequireDirective ->
      lineWithAutorequire True False


processAutorequireContent :: File.Input -> TransformM Text
processAutorequireContent autorequireContent = do
  modify $ \s -> s
     { tstLineTagPrepend = prependLineTag
     , tstAutorequire    = AutorequireDisabled
     }

  processed <- File.inputLines autorequireContent
     & mapM (process True)
     & fmap mconcat

  modify $ \s -> s { tstLineTagPrepend = prependLineTag }
  pure processed


renderImport :: Bool -> File.LineTag -> RequireInfo -> TransformM Text
renderImport filterImports line RequireInfo {..} = do
    tst <- get
    let (res, prep) =
          if filterImports && tstHostModule tst == Just riFullModuleName
             then ("", prependLineTag)
             else (typesImport <> renderLineTag line <> qualifiedImport, ignoreLineTag)
    put tst { tstLineTagPrepend = prep }
    pure $ tstLineTagPrepend tst line res
  where
    typesImport = unwords
      [ "import"
      , unModuleName riFullModuleName
      , "(" <> riImportedTypes <> ")"
      ] <> "\n"
    qualifiedImport = unwords
      [ "import qualified"
      , unModuleName riFullModuleName
      , "as"
      , riModuleAlias
      ] <> "\n"
