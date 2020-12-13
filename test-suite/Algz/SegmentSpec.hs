module Algz.SegmentSpec (spec) where
import           Algz.Segments
import           Test.Hspec

spec ∷ Spec
spec = do
  describe "short segment with max sum, mss" $ do
    it "run through typical example" $ do
      Algz.Segments.mss 3 [1, -2, 3, 0, -5, 3, -2, 3, -1] `shouldBe` [3,-2,3]
    it "is empty on a negatives only list" $ do
      mss 2 [-1,-2,-4,-3] `shouldBe` []
    it "is sum of list if list is positives" $ do
      let xs = [4,3,2,8]
      mss (length xs) xs `shouldBe` xs
    it "is the max value in the list, for short length of 1, list has one positive" $ do
      mss 1 [3,-1,4,-2,1,9] `shouldBe` [9]

  describe "prefix with max sum, msp" $ do
    it "first three are positive, segment max length is five" $ do
      msp 5 [2,1,3,-2,-1,0] `shouldBe` [2,1,3]
    it "sums first five despite negatives, segment max length is five" $ do
      msp 5 [2,-1,3,-2,3,10] `shouldBe` [2,-1,3,-2,3]
    it "is the head of the list if its positive" $ do
      let xs = [2,3,4]
      msp 1 xs `shouldBe` [head xs]
    it "is empty if the head is negative (for segment max length of 1)" $ do
      msp 1 [-1,2,-3] `shouldBe` []
