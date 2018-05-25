-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.
import Universum

import System.Environment as System

import qualified Require

main :: IO ()
main = do
  (_:inFile:outFile:_) <- System.getArgs
  prepended <- readFile "/home/nick/.require"
  content <- readFile inFile
  writeFile outFile $ Require.transform (Require.FileName $ toText inFile) prepended content
