module Profiler where

-- | Threshold for triggering coherence event
coherenceEventThreshold :: Double
coherenceEventThreshold = 0.5

-- | Calculate Relative Energy Efficiency (ε)
calculateRelativeEfficiency :: Double -> Double -> Double -> Double
calculateRelativeEfficiency totalPhi cycleDuration currentEntropy =
    let 
        coherenceTarget = 3.0
        coherenceError  = abs (totalPhi - coherenceTarget)
        timeFactor      = max 1e-6 cycleDuration
        rawEfficiency   = (1.0 + (1.0 / (coherenceError + 1.0))) / timeFactor
    in  rawEfficiency / 1000.0  -- normalized

-- | Check for coherence event
isCoherenceEvent :: Double -> Bool
isCoherenceEvent coherence = coherence > coherenceEventThreshold

-- | Generate dynamic plot title
getPlotTitle :: Int -> String
getPlotTitle n = "MythEngine Phase 4: Coherence-Efficiency Analysis (Cycle " ++ show n ++ ")"


