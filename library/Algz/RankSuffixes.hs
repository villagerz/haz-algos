module Algz.RankSuffixes where

import           Prelude.Unicode ((∘))

tails ∷ [a] → [[a]]
tails [] = []
tails xs = xs : tails (tail xs)

rank ∷ Ord a ⇒ [a] → [Int]
rank xs = map (\x → length (filter (< x) xs)) xs

ranktails ∷ Ord a ⇒ [a] → [Int]
ranktails = rank ∘ tails

(<<) ∷  [Int] → [Int] → [Int]
(<<) xs ys = rank (zip xs ys)

-- | instead of looking at (<) lexicographically
-- in ranktails, look at (<k) which is to compare
-- first k ∈ents only
rats ∷ Ord a ⇒ Int → [a] → [Int]
rats k = rank ∘ map (take k) ∘ tails
