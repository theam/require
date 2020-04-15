{-# LANGUAGE FlexibleContexts #-}
module Require.Transform where

import Control.Category ((>>>))
import Control.Monad.Except
import Control.Monad.Writer.Strict
import Data.DList (DList)
import qualified Data.Text as Text
import Relude
import Require.Error (Error(..))
import qualified Require.File as File
import qualified Require.Parser as Parser
import Require.Types


-- | The monad stack used during transformation:
--
-- * @'StateT' 'TransformState'@ to keep track of whether to render the next
--   line tag, the module's name etc.
-- * @'WriterT' ('DList' 'Text')@ to collect the output lines. Instead of
--   haskell's built-in list type we use 'DList' because of it's /O(1)/ append
--   operation.
-- * @'Either' 'Error'@ to return errors.
type TransformM =
  StateT TransformState
    (WriterT (DList Text)
      (Either Error))


data TransformState = TransformState
  { tstLineTagOutput  :: File.LineTag -> TransformM ()
  , tstHostModule     :: !(Maybe ModuleName)
  , tstAutorequire    :: !(AutorequireMode File.Input)
  }


-- | Outputs a single line.
output :: Text -> TransformM ()
output = tell . pure

-- | Outputs the pragma representation of the given line tag.
renderLineTag :: File.LineTag -> TransformM ()
renderLineTag (File.LineTag (File.Name fn) (File.LineNumber ln)) =
  output $ "{-# LINE " <> show ln <> " \"" <> fn <> "\" #-}"

-- | Ignore the given line tag, specifically don't render it.
ignoreLineTag :: File.LineTag -> TransformM ()
ignoreLineTag = const (pure ())


transform :: AutorequireMode File.Input -> File.Input -> Either Error [Text]
transform autorequire =
  -- TODO:
  --  * if the mapM overhead is too much maybe use a streaming library
  --  * there is no need to concatenate the whole output in memory, a lazy text would be fine
  --  * maybe we should check if tstAutorequired is set after processing
  File.inputLines
    >>> traverse_ (process False)
    >>> flip runStateT initialState
    >>> execWriterT
    >>> fmap toList
  where
    initialState = TransformState
      { tstLineTagOutput  = renderLineTag
      , tstHostModule     = Nothing
      , tstAutorequire    = autorequire
      }


process :: Bool -> (File.LineTag, Text) -> TransformM ()
process filterImports (tag, line) = do
  -- Uses 'tstLineTagOutput' to render the current lines tag if necessary.
  let useTagPrep = do
        tst <- get
        tstLineTagOutput tst tag
        put (tst { tstLineTagOutput = ignoreLineTag })

  let lineWithAutorequire isDirective autoCondition = do
        autoMode <- gets tstAutorequire
        case autoMode of
          AutorequireEnabled autoContent
            | isDirective || autoCondition -> do
                -- If this is an `autorequire` directive, ignore it. Otherwise
                -- output the line tag if necessary (useTagPrep) and then the
                -- line itself.
                unless isDirective (useTagPrep >> output line)
                processAutorequireContent autoContent

          AutorequireOnDirective (Just autoContent)
            | isDirective -> processAutorequireContent autoContent

          AutorequireOnDirective Nothing
            | isDirective -> throwError MissingRequiresFile

          _ | isDirective -> pure ()
            | otherwise   -> useTagPrep >> output line

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


processAutorequireContent :: File.Input -> TransformM ()
processAutorequireContent autorequireContent = do
  modify $ \s -> s
     { tstLineTagOutput  = renderLineTag
     , tstAutorequire    = AutorequireDisabled
     }

  traverse_ (process True) (File.inputLines autorequireContent)
  modify $ \s -> s { tstLineTagOutput = renderLineTag }


renderImport :: Bool -> File.LineTag -> RequireInfo -> TransformM ()
renderImport filterImports line RequireInfo {..} = do
    tst <- get
    if filterImports && tstHostModule tst == Just riFullModuleName
      then
        -- We skipped a line, therefore we need a line tag before outputting
        -- the next one.
        put (tst { tstLineTagOutput = renderLineTag })

      else
        tstLineTagOutput tst line
          >> output typesImport
          >> renderLineTag line
          >> output qualifiedImport
          >> put (tst { tstLineTagOutput = ignoreLineTag })
  where
    typesImport = unwords
      [ "import"
      , unModuleName riFullModuleName
      , "(" <> riImportedTypes <> ")"
      ]
    qualifiedImport = unwords
      [ "import qualified"
      , unModuleName riFullModuleName
      , "as"
      , riModuleAlias
      ]
