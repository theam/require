{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | This module contains functions for file handling. The names of the
-- functions and data types are chosen in a way to suggest a qualified import.
--
-- @
-- __import qualified__ Require.File __as__ File
-- @
module Require.File where

import Relude

-- | Wraps the name of a file as given by the user. Usually this corresponds to
-- the file's path.
newtype Name = Name Text

-- | Associates a file's contents with the path from which it was read.
data Input = Input
  { inputName :: Name,
    inputContent :: Text
  }

-- | A type-safe wrapper for line numbers.
newtype LineNumber = LineNumber Int
  deriving (Enum)

-- | Identifies a specific line in a file.
data LineTag = LineTag !Name !LineNumber

-- | Returns the 'LineTag' referencing the first line in a given 'FileInput'.
--
-- Note that the tag's line number is 1-based, which fits well with how GHC
-- understands @{-\# LINE ... #-}@ pragmas.
initialLineTag :: Input -> LineTag
initialLineTag inp = LineTag (inputName inp) (LineNumber 1)

-- | Returns a line tag from the same file but referencing the next line.
advanceLineTag :: LineTag -> LineTag
advanceLineTag (LineTag fn ln) = LineTag fn (succ ln)

-- | @read name@ reads the contents of the file identified by @name@.
read :: Name -> IO Input
read f = Input f <$> readFileText (nameToPath f)

-- | @write name content@ writes @content@ to the file identified by @name@.
write :: Name -> Text -> IO ()
write = writeFileText . nameToPath

-- | Splits the input into lines and annotates each with a 'LineTag'.
inputLines :: Input -> [(LineTag, Text)]
inputLines fi = zip lineTags contents
  where
    lineTags = iterate advanceLineTag (initialLineTag fi)
    contents = lines (inputContent fi)

-- | Returns the 'FilePath' corresponding a given 'Name'.
nameToPath :: Name -> FilePath
nameToPath (Name fp) = toString fp
