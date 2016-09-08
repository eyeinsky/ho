module ArbitraryRatio where

import Prelude

data ArbitraryRatio a = AR a a

instance (Num a) => Num (ArbitraryRatio a) where
  AR n d + AR n' d' = AR (n * d' + n' * d) (d * d')
  -- AR n d - AR n' d' =
  AR n d * AR n' d' = AR (n * n') (d * d')
  abs (AR n d) = AR (abs n) (abs d)
  signum = undefined -- :: a -> a
  negate (AR n d) = AR (negate n) d
  fromInteger n = AR (fromInteger n) 1
