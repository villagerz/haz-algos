module Algz.Unravel where
import           Prelude.Unicode ((∘))

unravels ∷ [a] → [[[a]]]
unravels = foldr (concatMap ∘ prefixes) [[]]

prefixes ∷ a → [[a]] → [[[a]]]
prefixes x []         = [[[x]]]
prefixes x (xs : xss) = [(x:xs):xss] ++ map (xs : ) (prefixes x xss)
