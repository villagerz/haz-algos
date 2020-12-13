-- | From Chapter 11 "Segments and Subsequences", Algorithm Design with Haskell
module Algz.Segments where
import           Data.List       (foldr1, inits, tails)
import           Prelude.Unicode ((∘), (≡), (≢), (≤), (≥), (⊥))

-- | Short segment, of max length n, that has max sum
mss ∷ Int → [Integer] → [Integer]
mss n  = maxWith sum ∘ map (msp n) ∘ tails

-- | Short segment, of max length n, that is a prefix and is max sum
msp ∷ Int → [Integer] → [Integer]
msp n = maxWith sum ∘ filter (short n) ∘ inits

short ∷ Int → [a] → Bool
short b xs = (length xs) ≤ b

segments ∷ [a] → [[a]]
segments = concatMap inits . tails

maxWith ∷ Ord b ⇒ (a → b) → [a] → a
maxWith f  = foldr1 (bigger f)
  where bigger g x y = if g x ≥ g y then x else y

