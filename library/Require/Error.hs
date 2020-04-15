module Require.Error where

import Control.Exception
import Relude
import qualified Require.File as File
import System.Console.ANSI
import System.IO


data Error
  = MissingRequiresFile
  | AutorequireImpossible
  deriving (Eq, Show)


describe :: Error -> [String]
describe MissingRequiresFile =
  [ "Discovered an `autorequire` directive but no `Requires` file was found."
  ]
describe AutorequireImpossible =
  [ "Unable to determine where to insert the autorequire contents."
  , "Use the `autorequire` directive to specify a location yourself."
  ]


die :: File.Name -> Error -> IO a
die (File.Name fn) e = do
  let outputHeaderColored = do
        hSetSGR stderr [SetConsoleIntensity BoldIntensity]
        hPutStr stderr (toString fn ++ ": ")
        hSetSGR stderr [SetColor Foreground Vivid Red]
        hPutStr stderr "error:\n"

  -- Don't mess up the terminal if there is an exception half-way through.
  outputHeaderColored `finally` hSetSGR stderr []

  let indent s = replicate 4 ' ' ++ s
  traverse_ (hPutStrLn stderr . indent) (describe e)
  exitFailure
