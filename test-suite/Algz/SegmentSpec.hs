module Algz.SegmentSpec (spec) where
import           Algz.Segments
import           Prelude.Unicode ((∘), (∧), (≡), (≢), (≤), (≥), (⊥))
import           Test.Hspec

spec ∷ Spec
spec = do
  describe "mss - short segment with max sum" $ do
    it "run through typical example" $ do
      Algz.Segments.mss 3 [1, -2, 3, 0, -5, 3, -2, 3, -1] `shouldBe` [3,-2,3]
    it "is empty on a negatives only list" $ do
      mss 2 [-1,-2,-4,-3] `shouldBe` []
    it "is sum of list if list is positives" $ do
      let xs = [4,3,2,8]
      mss (length xs) xs `shouldBe` xs
    it "is the max value in the list, for short length of 1, list has one positive" $ do
      mss 1 [3,-1,4,-2,1,9] `shouldBe` [9]

  describe "msp - prefix with max sum" $ do
    it "first three are positive, segment max length is five" $ do
      msp 5 [2,1,3,-2,-1,0] `shouldBe` [2,1,3]
    it "sums first five despite negatives, segment max length is five" $ do
      msp 5 [2,-1,3,-2,3,10] `shouldBe` [2,-1,3,-2,3]
    it "is the head of the list if its positive" $ do
      let xs = [2,3,4]
      msp 1 xs `shouldBe` [head xs]
    it "is empty if the head is negative (for segment max length of 1)" $ do
      msp 1 [-1,2,-3] `shouldBe` []

  describe "maxSumPreorder - preorde definition to find max sum at smallest length" $ do
    it "keeps shrter prefix whose sum is at least is big" $ do
      maxSumPreorder [2,3] [2,3,0,0] `shouldBe` True
    it "is false even is sum is larger but so is length" $ do
      maxSumPreorder [3,7] [3] `shouldBe` False
    it "is false even if length is shorter but sum is smaller" $ do
      maxSumPreorder [3] [7,2] `shouldBe` False
    it "is true even if prefixes don't match, just to be clear" $ do
      maxSumPreorder [5,4] [1,2] `shouldBe` True
    it "is true is sum is the same and length is the same" $ do
      maxSumPreorder [2,3] [3,2] `shouldBe` True

  describe "thinBy - for evert ∈ent in xs, there is a y ≤ x" $ do
    it "finds tuples of coordinates using cmp function" $ do
      thinBy fcmp [(1,2), (4,3), (2,3), (5,4), (3,1)] `shouldBe` [(1,2), (4,3), (5,4), (3,1)]

fcmp ∷ (Int, Int) → (Int, Int) → Bool
fcmp (a,b) (c,d) = (a ≥ c) ∧ (b ≤ d)
