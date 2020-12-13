module Algz.SegmentSpec (spec) where
import           Algz.Segments
import           Test.Hspec

spec ∷ Spec
spec = do
  describe "short segment with max sum, mss" $ do
    it "run through typical example" $ do
      Algz.Segments.mss 3 [1, -2, 3, 0, -5, 3, -2, 3, -1] `shouldBe` [3,-2,3]
