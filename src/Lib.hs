-- Phase 9.1: Active Rebound Kick Implementation
module Lib where

import Control.Monad (replicateM_)
import System.Random
import Data.IORef
import Data.List (intercalate)
import Text.Printf (printf)

-- Configuration
maxCycles :: Int
maxCycles = 200
initialCoherenceThreshold :: Double
initialCoherenceThreshold = 1.5
reboundBoost :: Double
reboundBoost = 0.05   -- strength of rebound injection

data FieldState = FieldState {coherence :: Double, entropy :: Double, flux :: Double} deriving (Show)
data MetaState  = MetaState  {historicalCoherence :: [Double], coherenceAdjustment :: Double,
                              explorationAdjustment :: Double, reboundCount :: Int} deriving (Show)
data SimulationState = SimulationState {fieldState :: FieldState, cycleCount :: Int,
                                        metricHandle :: IORef [(Int, Double, Double, Double)],
                                        metaState :: MetaState}

initialState :: IO SimulationState
initialState = do
    metrics <- newIORef []
    return $ SimulationState
        { fieldState = FieldState 1.0 50.0 0.0
        , cycleCount = 0
        , metricHandle = metrics
        , metaState = MetaState [] 0.1 0.05 0
        }

randomDelta :: Double -> IO Double
randomDelta maxD = getStdRandom (randomR (-maxD, maxD))

generateEntropy :: FieldState -> Double -> IO FieldState
generateEntropy fs adj = do
    d <- randomDelta adj
    let newEntropy = max 1.0 (entropy fs + d)
    return fs {entropy = newEntropy, flux = abs d}

updateCoherence :: FieldState -> Double -> FieldState
updateCoherence fs threshold
    | coherence fs > threshold = fs {coherence = coherence fs + (0.5 * flux fs) / entropy fs}
    | otherwise                = fs {coherence = max 0.1 (coherence fs - (0.25 * flux fs))}

calculateMetrics :: FieldState -> (Double, Double)
calculateMetrics fs =
    let e = entropy fs * 30.0 + (1000.0 / coherence fs)
        eGW = e / 10000.0
    in (e, eGW)

updateMetaState :: SimulationState -> SimulationState
updateMetaState sim =
    let ms = metaState sim
        coh = coherence (fieldState sim)
        hist = take 5 (coh : historicalCoherence ms)
        avgC = sum hist / fromIntegral (length hist)
        change = maximum hist - minimum hist
        reboundCnt = reboundCount ms
        (newCAdj, newEAdj)
            | coh <= 0.1 = (0.1, 0.3)
            | change < 0.1 && avgC > 1.5 = (0.1, min 0.5 (explorationAdjustment ms * 1.5))
            | otherwise = (0.1, max 0.05 (explorationAdjustment ms * 0.95))
    in sim { metaState = MetaState hist newCAdj newEAdj reboundCnt }

runCycle :: SimulationState -> IO SimulationState
runCycle sim = do
    let fs = fieldState sim
        ms = metaState sim
    newFs' <- generateEntropy fs (explorationAdjustment ms)
    let newFs'' = updateCoherence newFs' initialCoherenceThreshold
        cyc = cycleCount sim + 1
        (eps, epsGW) = calculateMetrics newFs''

    modifyIORef (metricHandle sim) ((cyc, coherence newFs'', eps, epsGW):)
    let updatedSim = sim {fieldState = newFs'', cycleCount = cyc}
    let updatedMeta = updateMetaState updatedSim
    let currentCoh = coherence newFs''
        prevCoh = coherence fs

    -- Apply active rebound if stuck low twice
    let hist = historicalCoherence (metaState updatedMeta)
        isLowStuck = length hist >= 2 && head hist <= 0.1 && hist !! 1 <= 0.1
    (finalFs, finalMeta) <-
        if isLowStuck
           then do
               let boosted = newFs'' {coherence = coherence newFs'' + reboundBoost}
               putStrLn $ "REBOUND EVENT (Kick): Cycle " ++ show cyc ++
                          " | Coh boosted to " ++ printf "%.3f" (coherence boosted)
               return (boosted, (metaState updatedMeta){reboundCount = reboundCount (metaState updatedMeta) + 1})
           else return (newFs'', metaState updatedMeta)

    if cyc `mod` 10 == 0
       then printf "Cycle %4d | Coh: %.3f | ε: %.2e | ε_GW: %.2e *EVENT*\n" cyc (coherence finalFs) eps epsGW
       else return ()

    return updatedMeta { metaState = finalMeta } `seq` updatedSim { fieldState = finalFs, metaState = finalMeta }

runSimulation :: SimulationState -> IO ()
runSimulation sim
    | cycleCount sim >= maxCycles = summaryOutput sim
    | otherwise = runCycle sim >>= runSimulation

summaryOutput :: SimulationState -> IO ()
summaryOutput sim = do
    let ms = metaState sim
    allMetrics <- readIORef (metricHandle sim)
    let coherences = [c | (_, c, _, _) <- allMetrics]
        avgC = sum coherences / fromIntegral (length coherences)
    putStrLn "\n--- Phase 9.1: Rebound Verification Summary ---"
    printf "Total Rebound Events: %d\n" (reboundCount ms)
    printf "Average Coherence: %.3f\n" avgC
    putStrLn "-----------------------------------------------"

-- Phase 9.5: Rebound Summary Output
summaryOutput :: SimulationState -> IO ()
summaryOutput sim = do
    let ms = metaState sim
    allMetrics <- readIORef (metricHandle sim)
    let coherences = [c | (_, c, _, _) <- allMetrics]
        avgCoh = sum coherences / fromIntegral (length coherences)
    putStrLn "\n--- Phase 9: Rebound Verification Summary ---"
    printf "Total Rebound Events: %d\n" (reboundCount ms)
    printf "Average Coherence: %.3f\n" avgCoh
    putStrLn "---------------------------------------------"

main :: IO ()
main = do
    sim <- initialState
    runSimulation sim
    summaryOutput sim
