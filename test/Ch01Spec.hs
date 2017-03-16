module Ch01Spec (spec) where

import Test.Hspec
import Ch01 (sortWords, countRuns, sortRuns, showRun)

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
