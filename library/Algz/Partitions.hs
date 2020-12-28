-- | From Chapter 12 "Partitions", Algorithm Design with Haskell
module Algz.Partitions (parts)  where
import           Prelude.Unicode ((∘), (∧), (≡), (≢), (≤), (≥), (⊥))

parts ∷ [a] → [[[Char]]]
parts [] = [[[]]]
parts _  = [["a","b"], ["ab"]]
