module Algz.RankSuffixesSpec (spec) where
import           Algz.RankSuffixes
import           Data.Sort         (sort)
import           Prelude.Unicode   ((∘), (∧), (≡), (≢), (≤), (≥), (⊥))
import           Test.Hspec
import           Test.QuickCheck

spec ∷ Spec
spec = do
    describe "rank" $ do
      it "is equivalent to a rank of itself (rank ∘ rank ≡ rank)" $ property $
        \a → (rank ∘ rank ) a `shouldBe` (rank (a ∷ [Int]))
