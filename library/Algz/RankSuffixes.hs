module Algz.RankSuffixes where

import           Data.Array
import           Prelude.Unicode ((∘), (∨), (≡), (⊥))

tails ∷ [a] → [[a]]
tails [] = []
tails xs = xs : tails (tail xs)

-- | rank basic specification
rank ∷ Ord a ⇒ [a] → [Int]
rank xs = map (\x → length (filter (< x) xs)) xs



ranktails ∷ Ord a ⇒ [a] → [Int]
ranktails = applyUntil isperm rerankings ∘ rank

(<<) ∷  [Int] → [Int] → [Int]
(<<) xs ys = rank (zip xs ys)

-- | instead of looking at (<) lexicographically
-- in ranktails, look at (<k) which is to compare
-- first k ∈ents only
rats ∷ Ord a ⇒ Int → [a] → [Int]
rats k = rank ∘ map (take k) ∘ tails

applyUntil ∷ (a → Bool) → [a → a] → a → a
applyUntil _ [] x     = x
applyUntil p (f:fs) x = if p x then x else applyUntil p fs (f x)

isperm ∷ [Int] → Bool
isperm is = and (elems
                (accumArray (∨) False (0, n - 1) (zip is (repeat True)))
                )
            where n = length is

rerankings ∷ [[Int] → [Int]]
rerankings = map rerank (iterate (*2) 1)
  where rerank k rs = rs << shiftBy k rs

shiftBy ∷ Int → [Int] → [Int]
shiftBy k xs = (map (+k) (drop k xs) ) ++ [k-1, k-2 .. 0]

-- | partition sort
-- group candites with equal values into run
psort ∷ Ord b ⇒ [(a,b)] → [[a]]
psort xys = pass xys []
 where pass [] xss = xss
       pass (e@(x,y):xys') xss = step xys' [] [x] [] xss
         where step [] as bs cs xss' = pass as (bs : pass cs xss')
               step (e@(x,y'):xys'') as bs cs xss'
                 | y' < y = step xys'' (e:as) bs cs xss'
                 | y' ≡ y = step xys'' as (x:bs) cs xss'
                 | y' > y = step xys'' as bs (e:cs) xss'


