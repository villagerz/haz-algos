-- | From Algorith Design with Haskell Chapter 4
module Algz.BinSearch where

import           Data.Array
import           Prelude.Unicode ((∘), (≡), (≢), (≤), (⊥))

-- Greedy Algoz on grphs --
type Vertex = Int
type Edge = (Vertex, Vertex, Weight)
type Weight = Int
type Graph = ([Vertex], [Edge])
type Weights = Array (Vertex, Vertex) Weight
type State = (Links, [Vertex])
type Links = Array Vertex (Vertex, Distance)
type Distance = Int
type Tree = Graph
type Nat = Int


nodes (vs, _) = vs
edges (_, es) = es
source (u,_,_) = u

target ∷ Edge → Vertex
target (_,v,_) = v

weight ∷ Edge → Weight
weight (_,_,w) = w

-- assumes Vertex numbered starting from 1 to n
weights ∷ Graph → Weights
weights g = (array ((1,1), (n,n)) [((a,b), maxInt) | a ← [1 .. n], b ← [1 .. n]])
            // [((a,b),w) | (a,b,w) <- edges g]
  where n = length (nodes g)

parent ∷ Links → Vertex → Vertex
parent ls v = fst (ls ! v)

distance ∷ Links → Vertex → Distance
distance ls v = snd (ls ! v)

dijkstra ∷ Graph → Tree
dijkstra g = extract (apply (n-1) (gstep wa) (start n))
  where n = length (nodes g)
        wa = weights g

extract ∷ State → Tree
extract (ls, _) = (indices ls, [(u,v,w) | (v, (u,w)) ← assocs ls, v ≢ 1])

gstep ∷ Weights → State → State
gstep wa (ls, vs) = (ls', vs')
  where (d,v) = minimum [(distance ls v', v') | v' ← vs]
        vs' = filter (≢ v) vs
        ls' = accum better ls [(u,(v, sum' d (wa ! (v,u)))) | u ← vs']
          where sum' di w = if w ≡ maxInt then maxInt else di + w
                better (v1, d1) (v2, d2) = if d1 ≤ d2 then (v1,d1) else (v2,d2)

start ∷ Nat → State
start n = let ls = array (1,n) ((1,(1,0)):[(i, (i, maxInt)) | i ← [2 .. n] ])
          in  (ls, [1 .. n])

apply ∷ Nat → ( a → a ) → a → a
apply 0 _ = id
apply n f = f ∘ apply (n - 1) f

maxInt ∷ Int
maxInt = maxBound


