module Main where

import Prelude
import Data.Monoid
import Data.String
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.List as L
import qualified Data.Map as M

import Data.Functor.Identity
import Control.Monad.Writer

import qualified Options.Applicative as O
import System.Process (readProcess)

-- import Formatting
import qualified Data.Text.Format as F

-- * Command line arguments

data Config = Config
  -- histogram
  { buckets :: Integer
  , scale :: Either Integer Integer -- Either force $(tput lines)
  -- labels
  , showBuckets :: Bool
  , showCounts :: Bool
  } deriving (Show)

argp :: Config -> O.ParserInfo Config
argp def = O.info (O.helper <*> optParser) optProgDesc
  where
    optProgDesc = O.fullDesc
       <> O.header "ho"
       <> O.progDesc "<description>"
    optParser = pure Config
       <*> optAuto 'b' "buckets" "number of buckets" (O.value (buckets def))
       <*> optAuto 's' "scale" "scale counts to some integer" (O.value (scale def))
       <*> optAuto 'r' "show-buckets" "show buckets" (O.value (showBuckets def))
       <*> optAuto 'c' "show-counts" "show counts" (O.value (showCounts def))

main :: IO ()
main = do
  Config b s r c <- getDefaults
  freqs :: Counts Double <- readFreqs . lines <$> getContents
  TLIO.putStr $ sh r c s $ toBuckets b freqs

getDefaults :: IO Config
getDefaults = do
  cols <- read' "cols"
  lines <- read' "lines"
  let def = Config (lines - 1) (Right cols) False True
  args <- O.execParser $ argp def
  print args
  return args
  where
    read' what = read <$> readProcess "tput" [what] ""

-- * Histogram

type Counts a = M.Map a Integer
data Buckets a
  = Interval [((a, a), Integer)]
  | Discrete [(a, Integer)]

readFreqs :: (Read a, Ord a) => [String] -> Counts a
readFreqs = foldl (\m t -> M.insertWith (+) (read t) 1 m) M.empty

toBuckets :: (Fractional a, Ord a, Show a) => Integer -> Counts a -> Buckets a
toBuckets n rm = if toInteger (M.size rm) <= n
  then Discrete li
  else Interval $ f buckets li
  where
    li = M.toAscList rm
    keys = map fst li
    min = minimum keys
    max = maximum keys
    step = (max - min) / fromIntegral n
    points = iterate (+step) min
    buckets = takeWhile ((<= max) . snd) $ points `zip` tail points
    -- TODO buckets = let l = snd $ last buckets'
    --  in buckets' <> [(l, l + step)]
    f (bu@ (_, b) : xs) li = let
        (cur, rest) = L.partition ((<= b) . fst) li
      in (bu, sum (map snd cur)) : f xs rest
    f [] [] = []
    f a b = [] -- TODO error $ "a: " <> show a <> "\nb: " <> show b

-- * Show

sh :: (Real a, RealFrac a) => Bool -> Bool -> Either Integer Integer -> Buckets a -> TL.Text
sh showBuckets showCounts scaleC b = TL.unlines $ case b of
  Interval bm -> let
      (amax, bmax) = intervalLabels bm
      (aformat, amaxPadded) = mkFormat amax
      (bformat, bmaxPadded) = mkFormat bmax
      intervalLabel (a, b) = when showBuckets $ tell (TLB.toLazyText $ aformat a <> sep1 <> bformat b <> sep2)
      bucketLabelLength = toInteger $ (amaxPadded + bmaxPadded) + tlen sep1 + tlen sep2
    in map (row intervalLabel (scale bucketLabelLength)) bm

  Discrete bm -> let
      amax = discreteLabels bm
      (aformat, amaxPadded) = mkFormat amax
      discreteLabel a = when showBuckets $ tell (TLB.toLazyText $ aformat a <> sep2)
      bucketLabelLength = toInteger $ amaxPadded + tlen sep2
    in map (row discreteLabel (scale bucketLabelLength)) bm

  where
    sep1 = " - "
    sep2 = " "
    sep3 = "  "
    countBar scale c = TL.replicate (fromIntegral (scale c)) "*"
    countNum n c = TLB.toLazyText $ F.left n ' ' c <> sep3

    maxCount = maximum $ case b of Interval bm -> map snd bm; Discrete bm -> map snd bm
    countLength = length $ show maxCount
    countLabelLength = toInteger (countLength + tlen sep3)

    row :: forall b. (b -> W ()) -> (Integer -> Integer) -> (b, Integer) -> TL.Text
    row mkLabel scale (bucket, c) = execWriter $ do
      mkLabel bucket
      when showCounts $ tell (countNum countLength c)
      tell $ countBar scale c

    scale :: Integer -> Integer -> Integer
    scale bucketLabelLength = case scaleC of
      Left scaleTo -> let frac = fromInteger scaleTo / fromInteger maxCount
        in \cur -> floor (frac * fromInteger cur)
      Right termWidth -> let
        deduction = (if showBuckets then bucketLabelLength else 0) + (if showCounts then countLabelLength else 0)
        scaleTo = termWidth - deduction
        in if maxCount < scaleTo
          then id
          else let
              frac = fromInteger scaleTo / fromInteger maxCount
            in \cur -> floor (frac * fromInteger cur)

type W = WriterT TL.Text Data.Functor.Identity.Identity

tlen = fromIntegral . TL.length

intervalLabels bm = let
  (as, bs) = unzip $ map fst bm
  in (intSize as, intSize bs)

discreteLabels bm = intSize (map fst bm)

mkFormat n = let p = n + 3 in (F.left p ' ' . F.prec n, p)

intSize = length . show . maximum . map ceiling

-- * Helpers

-- optSw  = maker O.switch
-- optStr = maker O.strOption
optAuto = maker (O.option O.auto)

maker f short long help more = f
    $ O.short short
   <> O.long long
   <> O.help help
   <> more
