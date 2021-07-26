import qualified Data.Text as Text
import Relude
import qualified Require.Error as Error
import qualified Require.File as File
import qualified Require.Transform as Require
import Require.Types
import qualified Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec

main :: IO ()
main = do
  test <- testSpec "require" spec
  Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do
  let transformLines autoMode fileInput =
        case Require.transform autoMode fileInput of
          Left err -> do
            -- sadly expectationFailure is not polymorphic in its return type
            expectationFailure $ "Tranform failed: " ++ show err
            pure $ error "expectationFailure should have thrown"
          Right tr ->
            pure tr

      transformString autoMode =
         fmap toString . transform autoMode

      transform autoMode =
         fmap unlines . transformLines autoMode

  describe "the transformation" $ do
    it "transforms the 'require' keyword into a properly qualified import" $ do
      let input = "require Data.Text"
      let expected = "import qualified Data.Text as Text"
      actual <- transformString
            AutorequireDisabled
            (File.Input (File.Name "Foo.hs") input)
      actual `shouldContain` expected
    it "imports the type based on the module" $ do
      let input = "require Data.Text"
      let expected = "import Data.Text (Text)"
      actual <- transformString
            AutorequireDisabled
            (File.Input (File.Name "Foo.hs") input)
      actual `shouldContain` expected
    it "keeps the rest of the content intact" $ do
      let input = "module Foo where\nrequire Data.Text\nfoo = 42"
      let expectedStart = "{-# LINE 1"
      let expectedModule = "module Foo where"
      let expectedTypeImport = "import Data.Text (Text)"
      let expectedQualifiedImport = "import qualified Data.Text as Text"
      let expectedContent = "foo = 42\n"
      actual <- transformString
            AutorequireDisabled
            (File.Input (File.Name "Foo.hs") input)
      actual `shouldStartWith` expectedStart
      actual `shouldContain` expectedModule
      actual `shouldContain` expectedTypeImport
      actual `shouldContain` expectedQualifiedImport
      actual `shouldEndWith` expectedContent
    it "aliases the modules properly" $ do
      let input = "require Data.Text as Foo"
      let expectedTypeImport = "import Data.Text (Text)"
      let expectedQualifiedImport = "import qualified Data.Text as Foo"
      actual <- transformString
            AutorequireDisabled
            (File.Input (File.Name "Foo.hs") input)
      actual `shouldContain` expectedTypeImport
      actual `shouldContain` expectedQualifiedImport
    it "imports the types properly" $ do
      let input = "require Data.Text (Foo)"
      let expectedTypeImport = "import Data.Text (Foo)"
      let expectedQualifiedImport = "import qualified Data.Text as Text"
      actual <- transformString
            AutorequireDisabled
            (File.Input (File.Name "Foo.hs") input)
      actual `shouldContain` expectedTypeImport
      actual `shouldContain` expectedQualifiedImport
    it "imports the types and aliases the modules properly" $ do
      let input = "require Data.Text as Quux (Foo)"
      let expectedTypeImport = "import Data.Text (Foo)"
      let expectedQualifiedImport = "import qualified Data.Text as Quux"
      actual <- transformString
            AutorequireDisabled
            (File.Input (File.Name "Foo.hs") input)
      actual `shouldContain` expectedTypeImport
      actual `shouldContain` expectedQualifiedImport
    it "skips comments" $ do
      let input = "require Data.Text -- test of comments"
      let expected = "import Data.Text (Text)"
      actual <- transformString
            AutorequireDisabled
            (File.Input (File.Name "Foo.hs") input)
      actual `shouldContain` expected
    it "allows empty parentheses" $ do
      let input = "require Data.Text ()"
      let expected1 = "import Data.Text ()"
      let expected2 = "import qualified Data.Text as Text"
      actual <- transformLines
            AutorequireDisabled
            (File.Input (File.Name "Foo.hs") input)
      actual `shouldSatisfy` elem expected1
      actual `shouldSatisfy` elem expected2

  describe "require-mode" $ do
    it "keeps requires where the module is a substring of the filename" $ do
      -- Test case for https://github.com/theam/require/issues/20
      let fileInput = Text.unlines [ "module FooTest where", "require Foo" ]
      let expected1 = "import Foo (Foo)"
      let expected2 = "import qualified Foo as Foo"
      actual <- transformLines
            AutorequireDisabled
            (File.Input (File.Name "FooTests.hs") fileInput)
      actual `shouldSatisfy` elem expected1
      actual `shouldSatisfy` elem expected2

    describe "autorequire directive" $ do
      it "respects them" $ do
        let fileInput = Text.unlines [ "module Main where", "autorequire", "import B" ]
        let requireInput = Text.unlines [ "import A" ]
        actual <- transformLines
              (AutorequireOnDirective $ Just $ File.Input (File.Name "Requires") requireInput)
              (File.Input (File.Name "src/Foo/Bar.hs") fileInput)
        actual `shouldSatisfy` elem "module Main where"
        actual `shouldSatisfy` elem "import A"
        actual `shouldSatisfy` elem "import B"
        actual `shouldSatisfy` elemN 0 "autorequire"
      it "ignores them after the first one" $ do
        let fileInput = Text.unlines [ "autorequire", "autorequire" ]
        let requireInput = Text.unlines [ "import A" ]
        actual <- transformLines
              (AutorequireOnDirective $ Just $ File.Input (File.Name "Requires") requireInput)
              (File.Input (File.Name "src/Foo/Bar.hs") fileInput)
        actual `shouldSatisfy` elemN 1 "import A"
        actual `shouldSatisfy` elemN 0 "autorequire"
      it "throws an error if no requires file is provided" $ do
        let fileInput = "autorequire"
        let actual = Require.transform
              (AutorequireOnDirective Nothing)
              (File.Input (File.Name "Foo.hs") fileInput)
        actual `shouldBe` Left Error.MissingOptionalRequiresFile

  describe "autorequire-mode" $ do
    describe "inclusion after module directive" $ do
      let checkInclusion n fileInput = do
            let requireInput = "import A"
            actual <- transformLines
                  (AutorequireEnabled $ File.Input (File.Name "Requires") requireInput)
                  (File.Input (File.Name "src/Foo/Bar.hs") fileInput)
            actual `shouldSatisfy` elemN n requireInput

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
        let fileInput = unlines
              [ "class Foo a where"
              , "instance Foo x => Bar (Baz x) where"
              , "data Vec n a where"
              , "  Nil :: Vec 0 a"
              , "  Cons :: a -> Vec n a -> Vec (n + 1) a"
              ]
        let requireInput = unlines [ "import A" ]
        let actual = Require.transform
              (AutorequireEnabled $ File.Input (File.Name "Requires") requireInput)
              (File.Input (File.Name "Foo.hs") fileInput)
        actual `shouldBe` Left Error.AutorequireImpossible
      it "doesn't add after data/class/instance declarations split to multiple lines" $ do
        let fileInput = unlines
              [ "class Foo a -- some explanation here"
              , "  where"
              ]
        let requireInput = unlines [ "import A" ]
        let actual = Require.transform
              (AutorequireEnabled $ File.Input (File.Name "Requires") requireInput)
              (File.Input (File.Name "Foo.hs") fileInput)
        actual `shouldBe` Left Error.AutorequireImpossible

    describe "triggered using the autorequire directive" $ do
      it "can be triggered before without a module directive" $ do
        let fileInput = unlines [ "autorequire", "main = return ()" ]
        let requireInput = unlines [ "import A" ]
        let expected = unlines
              [ "{-# LINE 1 \"Requires\" #-}"
              , "import A"
              , "{-# LINE 2 \"Foo.hs\" #-}"
              , "main = return ()"
              ]
        actual <- transform
              (AutorequireEnabled $ File.Input (File.Name "Requires") requireInput)
              (File.Input (File.Name "Foo.hs") fileInput)
        actual `shouldBe` expected
      it "can't be retrigged after the module directive" $ do
        let fileInput = unlines [ "module Main where", "autorequire" ]
        let requireInput = unlines [ "import A" ]
        let expected = unlines
              [ "{-# LINE 1 \"Foo.hs\" #-}"
              , "module Main where"
              , "{-# LINE 1 \"Requires\" #-}"
              , "import A"
              ]
        actual <- transform
              (AutorequireEnabled $ File.Input (File.Name "Requires") requireInput)
              (File.Input (File.Name "Foo.hs") fileInput)
        actual `shouldBe` expected
      it "doesn't retrigger automatically after the module directive" $ do
        let fileInput = unlines [ "autorequire", "module Main where" ]
        let requireInput = unlines [ "import A" ]
        let expected = unlines
              [ "{-# LINE 1 \"Requires\" #-}"
              , "import A"
              , "{-# LINE 2 \"Foo.hs\" #-}"
              , "module Main where"
              ]
        actual <- transform
              (AutorequireEnabled $ File.Input (File.Name "Requires") requireInput)
              (File.Input (File.Name "Foo.hs") fileInput)
        actual `shouldBe` expected

    it "drops self-imports" $ do
      let fileInput = "module Foo.Bar where"
      let requireInput = "require Foo.Bar"
      let notExpected = "import Foo.Bar"
      actual <- transform
            (AutorequireEnabled $ File.Input (File.Name "Requires") requireInput)
            (File.Input (File.Name "src/Foo/Bar.hs") fileInput)
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
      actual <- transform
            (AutorequireEnabled $ File.Input (File.Name "Requires") requireInput)
            (File.Input (File.Name "Foo.hs") fileInput)
      actual `shouldBe` expected


-- | Checks if a given element is contained exactly @n@ times in the given list.
elemN :: Eq a => Int -> a -> [a] -> Bool
elemN n a =  (n ==) . length . filter (a ==)
