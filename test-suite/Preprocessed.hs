{-# OPTIONS_GHC -F -pgmF autorequirepp #-}
module Preprocessed where

import Prelude

foo :: Text -> Text
foo = Text.takeWhile (/= '.')

