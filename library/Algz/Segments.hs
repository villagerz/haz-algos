-- | From Chapter 11 "Segments and Subsequences", Algorithm Design with Haskell
module Algz.Segments (mss, maxSumPreorder, msp, thinBy)  where
import           Data.List       (foldr1, inits, tails)
import           Prelude.Unicode ((∘), (∧), (≡), (≢), (≤), (≥), (⊥))

-- | Short segment, of max length n, that has max sum
mss ∷ Int → [Integer] → [Integer]
mss n  = maxWith sum ∘ map concat ∘ scanr (opR n) []
  where opR b x xss = thinR x (cutR b xss)
        cutR m xss = if length (concat xss) ≡ m then init xss else xss
        thinR x xss = add [x] xss
          where add xs xss
                  | sum xs > 0 = xs : xss
                  | null xss = []
                  | otherwise = add (xs ++ head xss) (tail xss)

-- | Short segment, of max length n, that is a prefix and is max sum
msp ∷ Int → [Integer] → [Integer]
msp n = last ∘ foldr (op n) [[]]

op ∷ Int → Integer → [[Integer]] → [[Integer]]
op b x xss = [] : thin (map (x:) (cut b xss))
  where cut l yss = if length (last yss) ≡ l then (init yss) else yss
        thin  = dropWhile (\xs → sum xs ≤ 0)


short ∷ Int → [a] → Bool
short b xs = (length xs) ≤ b

segments ∷ [a] → [[a]]
segments = concatMap inits . tails

maxWith ∷ Ord b ⇒ (a → b) → [a] → a
maxWith f  = foldr1 (bigger f)
  where bigger g x y = if g x ≥ g y then x else y


maxSumPreorder ∷ (Ord a, Num a) ⇒ [a] → [a] → Bool
maxSumPreorder xs ys = (sum xs ≥ sum ys) ∧ (length xs ≤ length ys)

thinBy ∷ (a → a → Bool) → [a] → [a]
thinBy f  = foldr bump []
  where bump x [] = [x]
        bump x (y:ys)
          | f x y = x : ys
          | f y x = y : ys
          | otherwise = x : y : ys
