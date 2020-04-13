{-# LANGUAGE FlexibleContexts #-}
module Require.Transform where

import qualified Data.Text as Text
import Relude
import qualified Require.File as File
import qualified Require.Parser as Parser
import Require.Types


type LineTagPrepend = File.LineTag -> Text -> Text

data TransformState = TransformState
  { tstLineTagPrepend :: !LineTagPrepend
  , tstHostModule     :: !(Maybe ModuleName)
  , tstAutorequired   :: !Bool
  }


renderLineTag :: File.LineTag -> Text
renderLineTag (File.LineTag (File.Name fn) (File.LineNumber ln)) =
  "{-# LINE " <> show ln <> " \"" <> fn <> "\" #-}\n"


prependLineTag :: LineTagPrepend
prependLineTag = (<>) . renderLineTag

ignoreLineTag :: LineTagPrepend
ignoreLineTag = const id


transform :: Bool -> File.Input -> Maybe File.Input -> Text
transform autorequireEnabled input prepended =
  -- TODO:
  --  * if the mapM overhead is too much maybe use a streaming library
  --  * there is no need to concatenate the whole output in memory, a lazy text would be fine
  --  * maybe we should check if tstAutorequired is set after processing
  File.inputLines input
    & mapM (process (pure Nothing) autorequireEnabled prepended)
    & flip evalState initialState
    & mconcat
  where
    initialState = TransformState
      { tstLineTagPrepend = prependLineTag
      , tstHostModule     = Nothing
      , tstAutorequired   = False
      }

process
  :: State TransformState (Maybe ModuleName)
  -> Bool
  -> Maybe File.Input
  -> (File.LineTag, Text)
  -> State TransformState Text
process getHostModule autorequireEnabled prepended (tag, line) = do
  let useTagPrep text = do
        prep <- gets tstLineTagPrepend
        modify $ \s -> s { tstLineTagPrepend = ignoreLineTag }
        pure $ prep tag text

  let lineWithAutorequire cond
        | autorequireEnabled && cond = do
            -- Call useTagPrep before processAutorequireContent because the
            -- latter one modifies tstLineTagPrepend which the former one uses.
            line' <- useTagPrep $ line <> "\n"
            auto  <- processAutorequireContent prepended
            pure $ line' <> auto
        | otherwise =
            useTagPrep $ line <> "\n"

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
      lineWithAutorequire $ hasModule && hasWhere

    Just (ModuleDirective moduleName) -> do
      -- If there is already a module name, don't overwrite it.
      modify $ \s -> s { tstHostModule = tstHostModule s <|> Just moduleName }
      lineWithAutorequire hasWhere

    Just (RequireDirective ri) ->
      -- renderImport already prepends the line tag if necessary.
      renderImport getHostModule tag ri

    Just AutorequireDirective ->
      processAutorequireContent prepended


processAutorequireContent :: Maybe File.Input -> State TransformState Text
processAutorequireContent Nothing = pure ""
processAutorequireContent (Just autorequireContent) = do
  alreadyAutorequired <- gets tstAutorequired
  autorequireContent' <-
    if alreadyAutorequired
       then pure ""
       else do
           modify $ \s -> s
             { tstLineTagPrepend = prependLineTag
             , tstAutorequired = True
             }

           File.inputLines autorequireContent
             & mapM (process (gets tstHostModule) False Nothing)
             & fmap mconcat

  modify $ \s -> s { tstLineTagPrepend = prependLineTag }
  pure autorequireContent'


renderImport
  :: MonadState TransformState m
  => m (Maybe ModuleName)
  -> File.LineTag
  -> RequireInfo
  -> m Text
renderImport getHostModule line RequireInfo {..} = do
    mhostModule <- getHostModule
    lineTagPrep <- gets tstLineTagPrepend
    let (res, prep) =
          if mhostModule == Just riFullModuleName
             then ("", prependLineTag)
             else (typesImport <> renderLineTag line <> qualifiedImport, ignoreLineTag)
    modify $ \s -> s { tstLineTagPrepend = prep }
    pure $ lineTagPrep line res
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
