module Require where

import Universum

import qualified Data.Text as Text

newtype FileName = FileName Text

transform :: FileName -> Text -> Text
transform filename input =
  let
    foo = Text.lines input
          <&> (Text.splitOn "require")
          &   zip [1..]
          <&> (\(lineNumber, texts) -> convertIfNeeded filename lineNumber texts)
          &   Text.concat
  in foo


convertIfNeeded :: FileName -> Int -> [Text] -> Text
convertIfNeeded (FileName fn) lineNumber texts =
  case texts of
    ["", moduleName] ->
      let
        moduleLast = Text.reverse moduleName
                     & Text.takeWhile (/= '.')
                     & Text.reverse

        qualifiedImport = lineTag
                          <> "import qualified"
                          <> moduleName
                          <> " as "
                          <> moduleLast

        typeImport = lineTag
                     <> "import"
                     <> moduleName
                     <> " ("
                     <> moduleLast
                     <> ")"

        lineTag = "{-# LINE "
                  <> show lineNumber
                  <> " \""
                  <> fn
                  <> "\" #-}"
      in
        typeImport
        <> "\n"
        <> qualifiedImport
        <> "\n"

    otherList ->
      unlines otherList
