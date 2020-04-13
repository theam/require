{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
module Require where

import qualified Data.Text as Text
import Options.Generic
import Relude
import System.Directory
import qualified Require.File as File
import Require.Transform


data CommandArguments
  = CommandArguments Text Text Text
  deriving (Generic)

instance ParseRecord CommandArguments


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
  let transformed = transform autorequire input requires
  File.write outputFile transformed
