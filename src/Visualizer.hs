module Visualizer (runVisualization) where

import Text.Printf
import System.IO

-- Simple ASCII plotter for MythEngine metrics
runVisualization :: IO ()
runVisualization = do
    putStrLn "\n--- Phase 10: Visualization & Analysis ---"
    contents <- readFile "data/metrics.csv"
    let ls = drop 1 (lines contents)  -- skip header
        entries = [ (read c1 :: Int, read c2 :: Double, read c3 :: Double, read c4 :: Double)
                  | l <- ls
                  , let ws = wordsWhen (==',') l
                  , length ws >= 4
                  , let [c1,c2,c3,c4] = take 4 ws ]
    putStrLn "\nCycle | Coherence Φ | ε (Eff) | ε_GW"
    putStrLn "--------------------------------------"
    mapM_ (\(cy,coh,eps,gw) ->
              printf "%5d | %10.3f | %7.1f | %7.3f %s\n"
                     cy coh eps gw (if coh <= 0.11 then "<-- REBOUND" else ""))
          entries

    let avgCoh = mean [c | (_,c,_,_) <- entries]
    printf "\nAverage Coherence: %.3f\n" avgCoh
    putStrLn "--------------------------------------"
    putStrLn "End of Visualization Phase.\n"

-- Utility: split string by predicate
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
          where (w, s'') = break p s'

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)
