-- You can benchmark your code quickly and effectively with Criterion. See its
-- website for help: <http://www.serpentine.com/criterion/>.
import           Algz.Segments   (mss)
import           Criterion.Main

import           Data.List
import           Prelude.Unicode ((∘), (∧), (≡), (≢), (≤), (≥), (⊥))
import           System.Random

randomlist :: Int -> StdGen -> [Integer]
randomlist n = take n . unfoldr (Just . random)

randomlist' ∷ Int → StdGen → [Integer]
randomlist' n = take n ∘ randoms

main :: IO ()
main = do
  seed ← newStdGen
  let ten' = randomlist' 10 seed
  let ten = randomlist 10 seed
  let hunsies = randomlist 100 seed
  let thou = randomlist 1000 seed
  print ten'
  print ten
  print (take 10 hunsies)
  defaultMain [
    bgroup "non-tupled-" [bench "5,10" $ whnf (mss 5) ten
                         ,bench "5,100" $ whnf (mss 5) hunsies
                         ,bench "5,1000" $ whnf (mss 5) thou
                         ,bench "half,10" $ whnf (mss 5) ten
                         ,bench "half,100" $ whnf (mss 50) hunsies
                         ,bench "half,1000" $ whnf (mss 500) thou
                         ,bench "full,10" $ whnf (mss 10) ten
                         ,bench "full,100" $ whnf (mss 100) hunsies
                         ,bench "full,1000" $ whnf (mss 1000) thou]
    ]
