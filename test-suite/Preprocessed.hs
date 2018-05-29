{-# OPTIONS_GHC -F -pgmF autorequirepp #-}
module Preprocessed where

foo :: Text -> Text
foo = Text.takeWhile (/= '.')

