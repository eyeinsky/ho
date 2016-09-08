module Distribution where

import Prelude
import GHC.Real

import ArbitraryRatio

class Distribution a where
  quantiles :: Num a => Integer -> [a] -> [ArbitraryRatio a]
  quantileIndexes :: Integer -> [a] -> [Ratio Integer]

instance (Num a) => Distribution a where
  quantiles n xs = map nthOf (quantileIndexes n xs)
    where
      nthOf :: Num a => Ratio Integer -> ArbitraryRatio a
      nthOf ratio = let
          (a, b) = properFraction ratio :: (Integer, Ratio Integer)
          (x1 : ys') = drop (fromInteger a) xs :: Num a => [a]
          n = numerator b :: Integer
          d = denominator b :: Integer
        in if b > 0
          then let
            x2 = head ys' :: a
            in (AR x1 1) + AR ((x2 - x1) * fromInteger n) (fromInteger d)
          else (AR x1 1)
  quantileIndexes n xs = take (fromInteger (n-1)) $ iterate (+diff) diff
    where
      diff = toInteger (length xs - 1) % n
