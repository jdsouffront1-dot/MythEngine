module Adaptive (analyzeAndAdjust, AdaptiveResult(..)) where

import Data.List (transpose)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

-- result returned to Main so it can update parameters
data AdaptiveResult = AdaptiveResult
  { newMaxNodes :: Int
  , newExplorationScale :: Double
  , phiMean :: Double
  , mdMean  :: Double
  , phiMdCorr :: Double
  } deriving (Show)

-- safe read Double
readD :: String -> Maybe Double
readD = readMaybe

-- parse metrics CSV (expects header; totalPhi at col index 5, avgMD at index 6)
parseMetrics :: String -> [(Double, Double)]
parseMetrics contents =
  let ls = drop 1 (lines contents) -- drop header
      cols row = map (trim) (splitComma row)
      parsed row =
        case cols row of
          (_cycle:_ts:_dur:_nn:_ne:phiS:mdS:_) ->
            case (readD phiS, readD mdS) of
              (Just p, Just m) -> Just (p, m)
              _                -> Nothing
          _ -> Nothing
  in mapMaybe parsed ls

-- basic CSV split (no quotes handling ? fine for our generated file)
splitComma :: String -> [String]
splitComma [] = [""]
splitComma s =
  let go acc cur [] = reverse (reverse cur : acc)
      go acc cur (',':xs) = go (reverse cur : acc) "" xs
      go acc cur (c:xs) = go acc (c:cur) xs
  in go [] "" s

trim :: String -> String
trim = f . f where f = reverse . dropWhile (== ' ')

mean :: [Double] -> Double
mean [] = 0
mean xs = sum xs / fromIntegral (length xs)

-- Pearson correlation (safe)
pearson :: [Double] -> [Double] -> Double
pearson xs ys
  | n == 0 = 0
  | denom == 0 = 0
  | otherwise = cov / denom
  where
    n = fromIntegral (length xs)
    mx = mean xs
    my = mean ys
    cov = sum (zipWith (\a b -> (a - mx) * (b - my)) xs ys)
    sx = sqrt (sum (map (\a -> (a - mx) ^ 2) xs))
    sy = sqrt (sum (map (\b -> (b - my) ^ 2) ys))
    denom = sx * sy

-- compute a simple linear trend (difference between means of halves)
trend :: [Double] -> Double
trend xs
  | n < 2 = 0
  | otherwise = mean (drop k xs) - mean (take k xs)
  where
    n = length xs
    k = max 1 n `div` 2

-- Core analyze function:
-- reads csv contents, computes mean phi, mean md, correlation and simple trend on phi
-- returns AdaptiveResult with suggested newMaxNodes and exploration scale
analyzeAndAdjust :: String       -- contents of metrics.csv
                 -> Int          -- currentMaxNodes (so we can propose near value)
                 -> AdaptiveResult
analyzeAndAdjust contents currentMax =
  let pairs = parseMetrics contents
      (phis, mds) = unzip pairs
      meanPhi = mean phis
      meanMd  = mean mds
      corr    = pearson phis mds
      phiTrend = trend phis
      -- rule-set (meta-dynamical):
      --  * if phi trend is rising and corr positive -> increase maxNodes moderately
      --  * if phi trend falling and corr negative -> decrease maxNodes
      --  * exploration scale grows when correlation is weak (system uncertain)
      inc = if phiTrend > 0 && corr > 0.15 then 5 else 0
      dec = if phiTrend < 0 && corr < -0.15 then -5 else 0
      baseProposal = currentMax + inc + dec
      safeClamp x = max 5 (min 200 x)
      proposal = safeClamp baseProposal
      explorationScale = if abs corr < 0.2 then 1.5 else 1.0
  in AdaptiveResult (fromIntegral proposal) explorationScale meanPhi meanMd corr


