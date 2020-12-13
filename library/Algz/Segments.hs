-- | From Chapter 11 "Segments and Subsequences", Algorithm Design with Haskell
module Algz.Segments where
import           Data.List       (foldr1, inits, tails)
import           Prelude.Unicode ((∘), (≡), (≢), (≤), (≥), (⊥))


mss ∷ Int → [Integer] → [Integer]
mss n  = maxWith sum ∘ filter (short n) ∘ segments

short ∷ Int → [a] → Bool
short b xs = (length xs) ≤ b

segments ∷ [a] → [[a]]
segments = concatMap inits . tails

maxWith ∷ Ord b ⇒ (a → b) → [a] → a
maxWith f  = foldr1 (bigger f)
  where bigger g x y = if g x ≥ g y then x else y

