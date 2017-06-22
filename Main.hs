module Main where

import Pr
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Scientific as Sc

import qualified Options.Applicative as O
import System.Process (readProcess)

import qualified Data.Text.Format as F

import Distribution
import Histogram

-- * Command line arguments

data Config = Config
  -- histogram
  { buckets :: Integer
  , scale :: Either Integer Integer -- Either force $(tput lines)
  -- labels
  , hideBuckets :: Bool
  , hideCounts :: Bool
  , input :: String
  } deriving (Show)

argp :: Config -> O.ParserInfo Config
argp def = O.info (O.helper <*> optParser) optProgDesc
  where
    optProgDesc = O.fullDesc
       <> O.header "ho"
       <> O.progDesc "Make simple histograms on the terminal from numeric input"
    optParser = pure Config
       <*> optAuto 'b' "buckets" "number of buckets" (O.value (buckets def))
       <*> optAuto 's' "scale" "scale counts to some integer" (O.value (scale def))
       <*> optSw 'r' "hide-buckets" "hide buckets" mempty
       <*> optSw 'c' "hide-counts" "hide counts" mempty
       <*> O.argument O.str (O.metavar "FILE")

main :: IO ()
main = do
  Config b s r c f <- getDefaults
  file <- if f == "-" then getContents else readFile f
  let
    (freqs :: Counts Sc.Scientific, errors) = readFreqs . lines $ file
    info = process freqs
  TLIO.putStr . sh r c s $ toBuckets b $ freqs
  let realErrors = filter (not . all (== ' ')) errors
  when (length realErrors > 0) $ putStrLn $ "Couldn't parse these lines: " <> show realErrors

getDefaults :: IO Config
getDefaults = do
  cols <- read' "cols"
  lines <- read' "lines"
  let def = Config (lines - 1) (Right cols) False False "-"
  args <- O.execParser $ argp def
  print args
  return args
  where
    read' what = read <$> readProcess "tput" [what] ""

-- * Histogram

-- * Show

sh :: (Real a, RealFrac a) => Bool -> Bool -> Either Integer Integer -> Buckets -> TL.Text
sh hideBuckets hideCounts scaleC b = TL.unlines $ case b of
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
    showBuckets = not hideBuckets
    showCounts = not hideCounts
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

type W = WriterT TL.Text Identity

tlen = fromIntegral . TL.length

intervalLabels bm = let
  (as, bs) = unzip $ map fst bm
  in (intSize as, intSize bs)

discreteLabels bm = intSize (map fst bm)

mkFormat n = let p = n + 3 in (F.left p ' ' . F.prec n, p)

intSize = length . show . maximum . map ceiling

-- * Helpers

optSw  = maker O.switch
-- optStr = maker O.strOption
optAuto = maker (O.option O.auto)

maker f short long help more = f
    $ O.short short
   <> O.long long
   <> O.help help
   <> more

u = undefined
