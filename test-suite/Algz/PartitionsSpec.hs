module Algz.PartitionsSpec (spec) where
import           Algz.Partitions
import           Prelude.Unicode ((∘), (∧), (≡), (≢), (≤), (≥), (⊥))
import           Test.Hspec

spec ∷ Spec
spec = do
    describe "partitions : abc → [[a, b, c], [a, bc], [ab, c], [abc]]" $ do
      it "has an empty partition list if the list is empty" $ do
        parts ([]::[Int]) `shouldBe` [[[]]]
      it "simple two elements has two partitions: \"ab\" → [[\"a\",\"b\"], [\"ab\"]]" $ do
        parts "ab" `shouldBe` [["a","b"], ["ab"]]
