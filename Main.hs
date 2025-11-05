module Main where

import Sim
import Types
import System.Directory (createDirectoryIfMissing)

main :: IO ()
main = do
  -- prepare
  let out = "data/metrics.csv"
  createDirectoryIfMissing True "data"
  writeMetricsHeader out

  -- build a small network and run cycles
  let net = initNetwork 10      -- 10 nodes to start
  putStrLn "Starting MythEngine tiny simulation (50 cycles)..."
  runCycles 50 out net
  putStrLn $ "Done. Metrics written to: " ++ out