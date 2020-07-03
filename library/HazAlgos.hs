-- | An example module.
module HazAlgos where

import           Prelude.Unicode ((∘), (≤), (⊥))

data Tree = Leaf Int
    | Fork Tree Tree
    deriving Show

type Forest = [Tree]

-- the final algorithm combining rollup and foldrn insert
-- data ∈ent changed to be a pair (cost t, t)
mintree ∷ [Int] → Tree
mintree = foldl1 Fork ∘ map snd ∘ foldrn insert (wrap ∘ leaf)

leaf ∷ Int → (Int, Tree)
leaf x = (x, Leaf x)

fork ∷ (Num a, Ord a) ⇒ (a, Tree) → (a, Tree) → (a, Tree)
fork (a,u) (b,v) = (1 + max a b, Fork u v)


insert' ∷ Int → [Tree] → [Tree]
insert' x ts' = Leaf x : split x ts'
  where split _ [u] = [u]
        split lf (u : v : ts) = if lf `max` cost u < cost v
                               then u : v : ts
                               else split lf (Fork u v : ts)
        split _ [] = (⊥)

-- final method
insert ∷ Int → [(Int, Tree)] → [(Int, Tree)]
insert x ts' = leaf x : split x ts'
  where split _ [u] = [u]
        split lf (u : v : ts) = if lf `max` fst u < fst v
                               then u : v : ts
                               else split lf (fork u v : ts)
        split _ [] = (⊥)


mincostTree' ∷ [Int] → Tree
mincostTree' = minBy cost ∘ trees

mincostTree ∷ [Int] → Tree
mincostTree = rollup ∘ foldrn insert' (wrap ∘ Leaf)


trees'' ∷ [Int] → [Tree]
trees'' []     = (⊥)
trees'' [x]    = [Leaf x]
trees'' (x:xs) = concatMap (prefixes x) (trees'' xs)

trees' ∷ [Int] → [Tree]
trees' = foldrn (concatMap ∘ prefixes) (wrap ∘ Leaf)

wrap ∷ a → [a]
wrap x = [x]

trees ∷ [Int] → [Tree]
trees = map rollup ∘ forests

forests ∷ [Int] → [Forest]
forests = foldrn (concatMap ∘ prefixes') (wrap ∘ wrap ∘ Leaf)

rollup ∷ Forest → Tree
rollup = foldl1 Fork


prefixes' ∷ Int → Forest → [Forest]
prefixes' x ts = [Leaf x : rollup (take k ts) : drop k ts | k ← [1 .. length ts]]

prefixes ∷ Int → Tree → [Tree]
prefixes x t@(Leaf _) = [Fork (Leaf x) t]
prefixes x t@(Fork u v) = [Fork (Leaf x) t ] ++
                          [Fork u' v | u' ← prefixes x u]

foldrn ∷ (a → b → b) → (a → b) → [a] → b
foldrn _ _ []     = (⊥)
foldrn _ g [x]    = g x
foldrn f g (x:xs) = f x (foldrn f g xs)

minBy ∷ (Tree → Int) → [Tree] → Tree
minBy f = foldl1 (cmp f)
  where cmp g u v = if g u ≤ g v then u else v

cost ∷ Tree → Int
cost (Leaf x)   = x
cost (Fork l r) = 1 + (cost l `max` cost r)





main :: IO ()
main = return ()
