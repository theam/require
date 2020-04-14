module Require.Error where

import Relude


data Error
  = MissingRequiresFile
  | AutorequireImpossible
  deriving (Eq, Show)


describe :: Error -> String
describe MissingRequiresFile =
  "discovered an `autorequire` directive but no `Requires` file was found."
describe AutorequireImpossible =
  "unable to determine where to insert the autorequire contents.\n\
  \  Use the `autorequire` directive to specify a location yourself."
