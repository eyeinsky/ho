module Histogram where

import Pr
import qualified Data.Map as M
import qualified Data.List as L
import ArbitraryRatio
import Distribution


type Counts a = M.Map a Integer
data Buckets
  = Interval [((Rational, Rational), Integer)]
  | Discrete [(Rational, Integer)]
  deriving Show

readFreqs :: (Read a, Ord a) => [String] -> (Counts a, [String])
readFreqs ts = runWriter $ foldM f M.empty ts
  where
    f m t = case readMaybe t of
      Nothing -> tell [t] *> pure m
      Just n -> pure $ M.insertWith (+) n 1 m

data Info a = Info
  { counts :: Counts a
  , sum :: a
  , average :: Ratio a
  , quantiles_ :: [ArbitraryRatio a]
  }

process :: forall a. (Num a) => Counts a -> Info a
process m = Info m sum' (sum' :% fromIntegral totalCount) q
  where
    li =  M.toList m :: [(a, Integer)]
    sum' = L.sum . map (uncurry (*) . second fromInteger) $ li :: a
    totalCount = L.sum $ map snd li :: Integer

    orderedValues ((v, n) : rest) = replicate (fromInteger n) v <> orderedValues rest -- :: [a]
    orderedValues _ = []
    q = quantiles 4 (orderedValues li)

toBuckets :: (Fractional a, Ord a, Show a, Real a) => Integer -> Counts a -> Buckets
toBuckets n rm = if toInteger (M.size rm) <= n
  then Discrete li
  else let
      buckets = mkIntervals n (map fst li)
    in Interval $ f buckets li
  where
    li = map (first toRational) $ M.toAscList rm

mkIntervals n li = let
    min = toRational $ minimum li
    max = toRational $ maximum li
    step = (max - min) / fromIntegral n
    points = takeWhile (<= max) $ iterate (+step) min
    intervals = points `zip` tail points
  in takeWhile ((<= max) . snd) intervals

f :: (Ord a, Num b) => [(a, a)] -> [(a, b)] -> [((a, a), b)]
f (bu@ (_, b) : xs) li = let
    (cur, rest) = L.partition ((<= b) . fst) li
  in (bu, L.sum (map snd cur)) : f xs rest
f [] [] = []
f a b = error "This should never happen"
