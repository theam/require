module Require.Error where

import Relude


data Error
  = MissingRequiresFile
  deriving (Eq, Show)


describe :: Error -> String
describe MissingRequiresFile = "Found an `autorequire` directive but no `Requires` file was found."
