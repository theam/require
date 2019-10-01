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
  it "autorequire does not lead to self-imports" $ do
    let fileInput = "module Foo.Bar where"
    let requireInput = "require Foo.Bar"
    let notExpected = "import Foo.Bar"
    let actual = Require.transform True
          (Require.FileInput (Require.FileName "src/Foo/Bar.hs") fileInput)
          (Just $ Require.FileInput (Require.FileName "Requires") requireInput)
    toString actual `shouldNotContain` notExpected
