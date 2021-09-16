-- | From Chapter 12 "Partitions", Algorithm Design with Haskell
module Algz.Partitions (parts)  where
import           Prelude.Unicode ((∘), (∧), (≡), (≢), (≤), (≥), (⊥))

type Segment a = [a]
type Partition a = [Segment a]

parts ∷ [a] → [Partition a]
parts [] = [[[]]]
parts xs = foldr (concatMap ∘ extend) [[]] xs

extend ∷ a → Partition a → [Partition a]
extend x [] = [[[x]]]
extend x p = [ [x]:p, glue x p]
  where glue a (h:ss) = (a:h):ss
