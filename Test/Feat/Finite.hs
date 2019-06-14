-- | A datatype of finite sequences
module Test.Feat.Finite (Finite (..), Index, fromFinite, finFin, sumSel, sumSel', removeMinimum, A(..)) where

import Control.Applicative
import Data.Semigroup
import Data.Monoid

type Index = Integer
data Finite a = Finite {fCard :: Index, fIndex :: Index -> a}

finEmpty = Finite 0 (\i -> error "index: Empty")

finUnion :: Finite a -> Finite a -> Finite a
finUnion f1 f2
  | fCard f1 == 0  = f2
  | fCard f2 == 0  = f1
  | otherwise      = Finite car sel where

  card1 = fCard f1
  card2 = fCard f2

  car = card1 + card2

  sel = sumSel [f1, f2]

instance Functor Finite where
  fmap f fin = fin{fIndex = f . fIndex fin}

instance Applicative Finite where
  pure = finPure
  a <*> b = fmap (uncurry ($)) (finCart a b)

instance Alternative Finite where
  empty = finEmpty
  (<|>) f1 f2 = mconcat [f1, f2]

instance Semigroup (Finite a) where
  (<>) f1 f2 = mconcat [f1, f2]

instance Monoid (Finite a) where
  mempty = finEmpty
  mappend f1 f2 = mconcat [f1, f2]
  mconcat xs = Finite
    (sum $ map fCard xs)
    (sumSel $ filter ((>0) . fCard) xs)

sumSel :: [Finite a] -> (Index -> a)
sumSel = sumSel' 0 0

data A = A | B | C | D | E deriving (Eq, Show)

sumSel' :: Integer -> Integer -> [Finite a] -> Index -> a
sumSel' offset height [f] i = fIndex f (i - offset + height)
sumSel' offset height fs i =
  let
    j = i - offset
    n = fromIntegral (length fs) :: Integer
    minCard = minimum (map fCard fs) :: Integer
    (valueIndex, partNumber) = j `quotRem` n
    currentPart = fs !! fromIntegral partNumber
    value = fIndex currentPart (valueIndex + height)
  in
    if j < n * minCard then
      value
    else
      sumSel' (offset + n * minCard) (height + minCard) (removeMinimum minCard fs) j

removeMinimum :: Integer -> [Finite a] -> [Finite a]
removeMinimum minCard []       = []
removeMinimum minCard (f:rest) =
  if fCard f == minCard then
    removeMinimum minCard rest
  else
    f : removeMinimum minCard rest

finCart :: Finite a -> Finite b -> Finite (a,b)
finCart f1 f2 = Finite car sel where
  car = fCard f1 * fCard f2
  sel i = let (q, r) = (i `quotRem` fCard f2)
    in (fIndex f1 q, fIndex f2 r)

finPure :: a -> Finite a
finPure a = Finite 1 one where
  one 0 = a
  one _ = error "Index out of bounds"


fromFinite :: Finite a -> (Index,[a])
fromFinite (Finite c ix) = (c,map ix [0..c-1])


instance Show a => Show (Finite a) where
  show = show . fromFinite

finFin :: Integer -> Finite Integer
finFin k | k <= 0 = finEmpty
finFin k = Finite k (\i -> i)
