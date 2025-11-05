module Synthesis (
    MetaState(..),
    initialMetaState,
    updateMetaState
) where

-- Meta-Adaptive Layer (Phase 7)
data MetaState = MetaState
    { historicalCoherence   :: [Double]
    , coherenceAdjustment   :: Double
    , explorationAdjustment :: Double
    } deriving (Show)

initialMetaState :: MetaState
initialMetaState = MetaState [] 0.1 0.05

updateMetaState :: MetaState -> Double -> MetaState
updateMetaState meta currentCoh =
    let newHist  = take 5 (currentCoh : historicalCoherence meta)
        avgCoh   = sum newHist / fromIntegral (length newHist)
        spread   = maximum newHist - minimum newHist
        (newCohAdj, newExpAdj)
          | spread < 0.1 && avgCoh > 1.5 = (0.1, min 0.5 (explorationAdjustment meta * 1.5))
          | otherwise                     = (0.1, max 0.05 (explorationAdjustment meta * 0.95))
    in meta { historicalCoherence   = newHist
            , coherenceAdjustment   = newCohAdj
            , explorationAdjustment = newExpAdj }
