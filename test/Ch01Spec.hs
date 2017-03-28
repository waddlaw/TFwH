module Ch01Spec (spec) where

import Test.Hspec
import Test.QuickCheck
import Ch01
  ( sortWords, countRuns, sortRuns, showRun
  , convert, convert1, convert2, convert2'
  , showEntry
  )

spec :: Spec
spec = do
  describe "Ex 1.3" $ do
    it "sortWords" $ do
      sortWords ["to", "be", "or", "not", "to", "be"] `shouldBe` ["be", "be", "not", "or", "to", "to"]
    it "countRuns" $ do
      countRuns ["be", "be", "not", "or", "to", "to"] `shouldBe` [(2, "be"), (1, "not"), (1, "or"), (2, "to")]
    it "sortRuns" $ do
      sortRuns [(2, "be"), (1, "not"), (1, "or"), (2, "to")] `shouldBe` [(2, "be"), (2, "to"), (1, "not"), (1, "or")]
    it "showRun" $ do
      showRun (2, "be") `shouldBe` " be: 2\n"

  describe "Ex 1.4" $ do
    it "convert" $ do
      convert 308000 `shouldBe` "three hundred and eight thousand"
      convert 369027 `shouldBe` "three hundred and sixty-nine thousand and twenty-seven"
      convert 369401 `shouldBe` "three hundred and sixty-nine thousand four hundred and one"
    it "convert1" $ do
      convert1 0 `shouldBe` "zero"
    it "convert2 == convert2'" $ property $
        \x -> do
          let px = getPositive (x::Positive Int)
          convert2 px == convert2' px

  describe "Ex 1.5" $ do
    it "convert" $ do
      convert 301123 `shouldBe` "three hundred and one thousand one hundred and twenty-three"

  describe "Ex 1.6" $ do
    it "showEntry" $ do
      showEntry ("eginrr", ["ringer"]) `shouldBe` "eginrr: ringer"
      showEntry ("eginrs", ["resign","signer","singer"]) `shouldBe` "eginrs: resign,signer,singer"
