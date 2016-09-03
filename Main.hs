module Main where

import Prelude
import Data.Monoid
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.List as L
import qualified Data.Map as M
-- import qualified Data.HashMap.Strict as HM
-- import qualified Data.Hashable as H

import qualified Options.Applicative as O

import Formatting

-- * Command line arguments

data Args = Args
  { buckets :: Integer
  , precision :: Int
  , scale :: Maybe Integer
  }

argp :: O.ParserInfo Args
argp = O.info (O.helper <*> optParser) optProgDesc

optProgDesc = O.fullDesc
   <> O.header "ho"
   <> O.progDesc "<description>"
optParser = pure Args
   <*> optAuto 'b' "buckets" "number of buckets" (O.value 10)
   <*> optAuto 'p' "precision" "number of significant digits to show in bucket labels" (O.value 4)
   <*> optAuto 's' "scale" "scale counts to some integer" (O.value Nothing)

main :: IO ()
main = do
  Args b p s <- O.execParser argp
  freqs :: Counts Double <- readFreqs . lines <$> getContents
  TLIO.putStr $ sh p s $ toBuckets b freqs

-- * Histogram

{-
type TokenMap a = HM.HashMap a Int

tokenFreqs :: (H.Hashable a, Eq a) => [a] -> TokenMap a
tokenFreqs = foldl (\m t -> HM.insertWith (+) t 1 m) HM.empty
-}

type Counts a = M.Map a Integer
type Buckets a = [((a, a), Integer)]

readFreqs :: (Read a, Ord a) => [String] -> Counts a
readFreqs = foldl (\m t -> M.insertWith (+) (read t) 1 m) M.empty

toBuckets :: (Fractional a, Ord a) => Integer -> Counts a -> Buckets a
toBuckets n rm = f buckets li
  where
    li = M.toAscList rm
    keys = map fst li
    min = minimum keys
    max = maximum keys
    step = (max - min) / fromIntegral n
    points = iterate (+step) min
    buckets = takeWhile ((<= max) . snd) $ points `zip` tail points
    f (bu@ (_, b) : xs) li = let
        (cur, rest) = L.partition ((<= b) . fst) li
      in (bu, sum (map snd cur)) : f xs rest
    f [] [] = []
    f a b = [] -- error "error" -- a: " <> show a <> "\nb: " <> show b

-- * Show

sh :: Real a => Int -> Maybe Integer -> Buckets a -> TL.Text
sh p ms bm = TL.unlines $ map row bm
  where
    row ((a, b), c) = format
      (prec p % " - " % prec p % " " % left 6 ' ' % " | ") a b c
      <> TL.replicate (fromIntegral (scale c)) "*"

    mc = maximum $ map snd bm
    scale = maybe id (\scale cur -> floor (fromIntegral (cur * scale) / fromIntegral mc)) ms :: Integer -> Integer

-- * Helpers

-- optSw  = maker O.switch
-- optStr = maker O.strOption
optAuto = maker (O.option O.auto)

maker f short long help more = f
    $ O.short short
   <> O.long long
   <> O.help help
   <> more
