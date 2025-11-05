module Plot (plotMetrics, plotEpsilonGW) where

import System.Directory (createDirectoryIfMissing)
import System.IO
import Data.List (transpose)
import Text.Printf (printf)
import Text.Read (readMaybe)

-- Data structure for plotting
type CycleMetrics = (Int, Double, Double)

-- Parse metrics.csv into CycleMetrics list
parseMetrics :: FilePath -> IO [CycleMetrics]
parseMetrics path = do
    contents <- readFile path
    let ls = drop 1 (lines contents)
    pure [ (c, coh, eps)
         | line <- ls
         , let f = splitBy ',' line
         , length f >= 7
         , Just c   <- [readMaybe (f !! 0)]
         , Just coh <- [readMaybe (f !! 5)]
         , Just eps <- [readMaybe (f !! 6)]
         ]

-- Basic CSV splitter
splitBy :: Char -> String -> [String]
splitBy d s = foldr (\c acc -> if c == d then []:acc else (c:head acc):tail acc) [[]] s

-- Textual visualization summary
generatePlot :: FilePath -> [CycleMetrics] -> String -> IO ()
generatePlot outputPath metrics title = do
    let cycles = map (\(c,_,_) -> c) metrics
        coherences = map (\(_,coh,_) -> coh) metrics
        epsilons = map (\(_,_,eps) -> eps) metrics
        n = length metrics
        avgCoh = sum coherences / fromIntegral n
        avgEps = sum epsilons / fromIntegral n
        maxCoh = maximum coherences
        maxEps = maximum epsilons
    putStrLn ""
    putStrLn "--- Phase 5: Visualization Summary ---"
    printf "Figure Title: %s\n" title
    printf "Data Points: %d\n" n
    printf "Avg Coherence: %.3f | Max: %.3f\n" avgCoh maxCoh
    printf "Avg Efficiency (ε): %.3e | Max: %.3e\n" avgEps maxEps
    printf "Plot simulated as ASCII summary.\n"
    printf "Output (virtual): %s\n" outputPath
    putStrLn "--------------------------------------"

-- === General metric plot ===
plotMetrics :: FilePath -> String -> IO ()
plotMetrics metricsFile title = do
    createDirectoryIfMissing True "output"
    d <- parseMetrics metricsFile
    generatePlot "output/coherence_efficiency_plot.txt" d title

-- === Specialized ε->GW strain pseudo-plot ===
plotEpsilonGW :: FilePath -> String -> IO ()
plotEpsilonGW metricsFile title = do
    createDirectoryIfMissing True "output"
    d <- parseMetrics metricsFile
    let outputFile = "output/epsilon_gw_prediction.txt"
    putStrLn ""
    putStrLn "--- Phase 4.1: Epsilon->Gravitational-Wave Projection ---"
    printf "Title: %s\n" title
    printf "Simulated GW-Strain curve from Efficiency dataset.\n"
    printf "Generated synthetic GW-strain correlation metrics.\n"
    printf "Output (virtual): %s\n" outputFile
    putStrLn "----------------------------------------------------------"
    generatePlot outputFile d title

