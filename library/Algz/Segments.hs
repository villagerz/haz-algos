-- | From Chapter 11 "Segments and Subsequences", Algorithm Design with Haskell
module Algz.Segments (mss, maxSumPreorder, msp, thinBy)  where
import           Data.Foldable   (toList)
import           Data.List       (foldr1, inits, tails)
import qualified Data.Sequence   as DS
import           Prelude.Unicode ((∘), (∧), (≡), (≢), (≤), (≥), (⊥))

type Partition = (Sum, Length, DS.Seq Segment)
type Segment = (Sum, Length, [Integer] → [Integer])
type Sum = Integer
type Length = Nat
type Nat = Int

-- | Short segment, of max length n, that has max sum
mss' ∷ Int → [Integer] → [Integer]
mss' n  = maxWith sum ∘ map concat ∘ scanr (opR n) []
  where opR b x xss = thinR x (cutR b xss)
        cutR m xss = if length (concat xss) ≡ m then init xss else xss
        thinR x xss = add [x] xss
          where add xs xss
                  | sum xs > 0 = xs : xss
                  | null xss = []
                  | otherwise = add (xs ++ head xss) (tail xss)

mss ∷ Int → [Integer] → [Integer]
mss b = extract ∘ maxWith sumP ∘ scanr (opP b) emptyP

extract ∷ Partition → [Integer]
extract = concatMap (flip segS []) ∘ toList ∘ segsP

opP ∷ Length → Integer → Partition → Partition
opP b x xss = thinP x (cutP b xss)
  where cutP n yss = if lenP yss ≡ n then initP yss else yss
        initP (s, k, zss) = let (t, m, _ ) = lastSq zss
                                in (s-t, k-m, DS.take (DS.length zss - 1) zss)

thinP ∷ Integer → Partition → Partition
thinP x xss = add (x, 1, ([x]++)) xss

add ∷ Segment → Partition → Partition
add xs xss | sumS xs > 0 = consP xs xss
           | lenP xss ≡ 0 = emptyP
           | otherwise = add (catS xs (headP xss)) (tailP xss)

lastSq ∷ DS.Seq Segment → Segment
lastSq s = case DS.viewr s of
             _ DS.:> a → a
             DS.EmptyR → (0,0,id)

consP ∷ Segment -> Partition → Partition
consP xs (s,l,xss) = (sumS xs + s , lenS xs + l , xs DS.<| xss)

headP ∷ Partition → Segment
headP p = headSQ (segsP p)

headSQ ∷ DS.Seq Segment → Segment
headSQ DS.Empty      = (0,0, id)
headSQ (a DS.:<| _ ) = a

tailP ∷ Partition → Partition
tailP p = let (hs, hl, _ ) = headP p
              in (sumP p - hs, lenP p - hl, tailSQ ∘ segsP $ p)

tailSQ ∷ DS.Seq Segment → DS.Seq Segment
tailSQ DS.Empty        = (⊥)
tailSQ (_ DS.:<| tail) = tail

catS ∷ Segment → Segment → Segment
catS x y = (sumS x + sumS y, lenS x + lenS y, (segS x) ∘ (segS y))

emptyP ∷ Partition
emptyP = (0,0,DS.Empty)

sumP ∷ Partition → Sum
sumP (s, _, _) = s

lenP ∷ Partition → Length
lenP (_, l, _) = l

segsP ∷ Partition → DS.Seq Segment
segsP (_, _, s) = s

sumS ∷ Segment → Sum
sumS (s, _, _) = s

lenS ∷ Segment → Length
lenS (_,l,_) = l

segS ∷ Segment → ([Integer] → [Integer])
segS(_,_,f) = f

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
