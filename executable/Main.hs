-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.
import Universum

import Options.Generic

import qualified Require

data CommandArguments =
  CommandArguments Text Bool Text Text
  deriving Generic

instance ParseRecord CommandArguments


main :: IO ()
main = do
  CommandArguments _ autorequire inputFile outputFile <- getRecord "Require Haskell preprocessor" :: IO CommandArguments
  prepended <- readFile "/home/nick/.require"
  content <- readFile (toString inputFile)
  writeFile
    (toString outputFile)
    (Require.transform
      autorequire
      (Require.FileName inputFile)
      prepended
      content)
