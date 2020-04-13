{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
module Require where

import qualified Data.Text as Text
import Options.Generic
import Relude
import System.Directory
import qualified Require.File as File
import qualified Require.Parser as Parser
import Require.Types


data CommandArguments
  = CommandArguments Text Text Text
  deriving (Generic)

instance ParseRecord CommandArguments


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


findRequires :: IO (Maybe File.Name)
findRequires = do
  currentDir <- getCurrentDirectory
  files <- getDirectoryContents currentDir
  let textFiles = fmap toText files
  return $ File.Name . head <$> nonEmpty (filter (Text.isSuffixOf "Requires") textFiles)


requireMain :: IO ()
requireMain = do
  CommandArguments inputFile _ outputFile <- getRecord "Require Haskell preprocessor" :: IO CommandArguments
  run False Nothing (File.Name inputFile) (File.Name outputFile)

autorequireMain :: IO ()
autorequireMain = do
  CommandArguments inputFile _ outputFile <- getRecord "Require Haskell preprocessor" :: IO CommandArguments
  requiresFile <- findRequires
  case requiresFile of
    Nothing -> die "There is no Requires file in the system"
    Just _  -> run True requiresFile (File.Name inputFile) (File.Name outputFile)

run :: Bool -> Maybe File.Name -> File.Name -> File.Name -> IO ()
run autorequire requiresFile inputFile outputFile = do
  input <- File.read inputFile
  requires <- traverse File.read requiresFile
  let transformed = Require.transform autorequire input requires
  File.write outputFile transformed

transform :: Bool -> File.Input -> Maybe File.Input -> Text
transform autorequireEnabled input prepended =
  -- TODO:
  --  * if the mapM overhead is too much maybe use a streaming library
  --  * there is no need to concatenate the whole output in memory, a lazy text would be fine
  --  * maybe we should check if tstAutorequired is set after processing
  File.inputLines input
    & mapM (process (pure Nothing))
    & flip evalState initialState
    & Text.concat
  where
    initialState = TransformState
      { tstLineTagPrepend = prependLineTag
      , tstHostModule     = Nothing
      , tstAutorequired   = False
      }

    process
      :: State TransformState (Maybe ModuleName)
      -> (File.LineTag, Text)
      -> State TransformState Text
    process getHostModule (tag, line) = do
      let useTagPrep text = do
            prep <- gets tstLineTagPrepend
            modify $ \s -> s { tstLineTagPrepend = ignoreLineTag }
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
          processAutorequireContent

    processAutorequireContent = do
      alreadyAutorequired <- gets tstAutorequired
      autorequireContent  <-
        if | alreadyAutorequired  -> pure ""
           | Nothing <- prepended -> pure ""
           | Just pr <- prepended -> do
               modify $ \s -> s
                 { tstLineTagPrepend = prependLineTag
                 , tstAutorequired = True
                 }

               File.inputLines pr
                 & mapM (process (gets tstHostModule))
                 & fmap Text.concat

      modify $ \s -> s { tstLineTagPrepend = prependLineTag }
      pure autorequireContent


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
