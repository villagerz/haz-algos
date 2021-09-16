module Algz.PartitionsSpec (spec) where
import           Algz.Partitions
import           Data.Sort       (sort)
import           Prelude.Unicode ((∘), (∧), (≡), (≢), (≤), (≥), (⊥))
import           Test.Hspec

spec ∷ Spec
spec = do
    describe "partitions : abc → [[a, b, c], [a, bc], [ab, c], [abc]]" $ do
      it "has an empty partition list if the list is empty" $ do
        parts ([]::[Int]) `shouldBe` [[[]]]
      it "simple two elements has two partitions: \"ab\" → [[\"a\",\"b\"], [\"ab\"]]" $ do
        sort(parts "ab") `shouldBe` sort([["a","b"], ["ab"]])
      it "has property that concat of partition gives the original list" $ do
        let l = "abracadabra"
        let ps = parts l
        length ps `shouldBe` (^) 2 (length l - 1)
        shouldBe ( all (\p → concat p ≡ l) ps) True
