import qualified Data.Text as Text
import Relude
import qualified Require
import qualified Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
  test <- testSpec "require" spec
  Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do
  let emptyRequiresFile =
        Just $ Require.FileInput (Require.FileName "Requires") ""

  describe "the transformation" $ do
    it "transforms the 'require' keyword into a properly qualified import" $ do
      let input = "require Data.Text"
      let expected = "import qualified Data.Text as Text"
      let actual = Require.transform False
            (Require.FileInput (Require.FileName "Foo.hs") input)
            emptyRequiresFile
      expected `Text.isInfixOf` actual
    it "imports the type based on the module" $ do
      let input = "require Data.Text"
      let expected = "import Data.Text (Text)"
      let actual = Require.transform False
            (Require.FileInput (Require.FileName "Foo.hs") input)
            emptyRequiresFile
      expected `Text.isInfixOf` actual
    it "keeps the rest of the content intact" $ do
      let input = "module Foo where\nrequire Data.Text\nfoo = 42"
      let expectedStart = "{-# LINE 1"
      let expectedModule = "module Foo where"
      let expectedTypeImport = "import Data.Text (Text)"
      let expectedQualifiedImport = "import qualified Data.Text as Text"
      let expectedContent = "foo = 42\n"
      let actual = toString $ Require.transform False
            (Require.FileInput (Require.FileName "Foo.hs") input)
            emptyRequiresFile
      actual `shouldStartWith` expectedStart
      actual `shouldContain` expectedModule
      actual `shouldContain` expectedTypeImport
      actual `shouldContain` expectedQualifiedImport
      actual `shouldEndWith` expectedContent
    it "aliases the modules properly" $ do
      let input = "require Data.Text as Foo"
      let expectedTypeImport = "import Data.Text (Text)"
      let expectedQualifiedImport = "import qualified Data.Text as Foo"
      let actual = toString $ Require.transform False
            (Require.FileInput (Require.FileName "Foo.hs") input)
            emptyRequiresFile
      actual `shouldContain` expectedTypeImport
      actual `shouldContain` expectedQualifiedImport
    it "imports the types properly" $ do
      let input = "require Data.Text (Foo)"
      let expectedTypeImport = "import Data.Text (Foo)"
      let expectedQualifiedImport = "import qualified Data.Text as Text"
      let actual = toString $ Require.transform False
            (Require.FileInput (Require.FileName "Foo.hs") input)
            emptyRequiresFile
      actual `shouldContain` expectedTypeImport
      actual `shouldContain` expectedQualifiedImport
    it "imports the types and aliases the modules properly" $ do
      let input = "require Data.Text as Quux (Foo)"
      let expectedTypeImport = "import Data.Text (Foo)"
      let expectedQualifiedImport = "import qualified Data.Text as Quux"
      let actual = toString $ Require.transform False
            (Require.FileInput (Require.FileName "Foo.hs") input)
            emptyRequiresFile
      actual `shouldContain` expectedTypeImport
      actual `shouldContain` expectedQualifiedImport
    it "skips comments" $ do
      let input = "require Data.Text -- test of comments"
      let expected = "import Data.Text (Text)"
      let actual = Require.transform False
            (Require.FileInput (Require.FileName "Foo.hs") input)
            emptyRequiresFile
      expected `Text.isInfixOf` actual
    it "allows empty parentheses" $ do
      let input = "require Data.Text ()"
      let expected1 = "import Data.Text ()"
      let expected2 = "import qualified Data.Text as Text"
      let actual = lines $ Require.transform False
            (Require.FileInput (Require.FileName "Foo.hs") input)
            emptyRequiresFile
      actual `shouldSatisfy` elem expected1
      actual `shouldSatisfy` elem expected2

  describe "require-mode" $ do
    it "respects autorequire directives" $ do
      let fileInput = Text.unlines [ "module Main where", "autorequire", "import B" ]
      let requireInput = Text.unlines [ "import A" ]
      let actual = Text.lines $ Require.transform False
            (Require.FileInput (Require.FileName "src/Foo/Bar.hs") fileInput)
            (Just $ Require.FileInput (Require.FileName "Requires") requireInput)
      actual `shouldSatisfy` elem "module Main where"
      actual `shouldSatisfy` elem "import A"
      actual `shouldSatisfy` elem "import B"

  describe "autorequire-mode" $ do
    describe "inclusion after module directive" $ do
      let checkInclusion n fileInput = do
            let requireInput = "import A"
            let actual = Text.lines $ Require.transform True
                  (Require.FileInput (Require.FileName "src/Foo/Bar.hs") fileInput)
                  (Just $ Require.FileInput (Require.FileName "Requires") requireInput)
            let elemN x = (n ==) . length . filter (x ==)
            actual `shouldSatisfy` elemN requireInput

      it "works for no export list" $ do
        checkInclusion 1 "module Main where"
      it "works for a one line export list" $ do
        checkInclusion 1 "module Main (Datatype(..), (*+), foo, Bar) where"
      it "works for a multi line export list" $ do
        checkInclusion 1 $ Text.unlines
          [ "module Main"
          , "  ( -- * Foo"
          , "  , Foo(..)"
          , "  , (*+)"
          , "  , -- ** Bar"
          , "  foo, Bar"
          , "  ) where"
          ]
      it "doesn't add after data/class/instance declarations" $ do
        checkInclusion 0 $ Text.unlines
          [ "class Foo a where"
          , "instance Foo x => Bar (Baz x) where"
          , "data Vec n a where"
          , "  Nil :: Vec 0 a"
          , "  Cons :: a -> Vec n a -> Vec (n + 1) a"
          ]
      it "doesn't add after data/class/instance declarations split to multiple lines" $ do
        checkInclusion 0 $ Text.unlines
          [ "class Foo a -- some explanation here"
          , "  where"
          ]

    it "drops self-imports" $ do
      let fileInput = "module Foo.Bar where"
      let requireInput = "require Foo.Bar"
      let notExpected = "import Foo.Bar"
      let actual = Require.transform True
            (Require.FileInput (Require.FileName "src/Foo/Bar.hs") fileInput)
            (Just $ Require.FileInput (Require.FileName "Requires") requireInput)
      toString actual `shouldNotContain` notExpected
    it "adds LINE pragmas around the Requires contents" $ do
      let fileInput = Text.unlines [ "module Main where", "import B" ]
      let requireInput = Text.unlines [ "import A" ]
      let expected = Text.unlines
            [ "{-# LINE 1 \"Foo.hs\" #-}"
            , "module Main where"
            , "{-# LINE 1 \"Requires\" #-}"
            , "import A"
            , "{-# LINE 2 \"Foo.hs\" #-}"
            , "import B"
            ]
      let actual = Require.transform True
            (Require.FileInput (Require.FileName "Foo.hs") fileInput)
            (Just $ Require.FileInput (Require.FileName "Requires") requireInput)
      actual `shouldBe` expected
