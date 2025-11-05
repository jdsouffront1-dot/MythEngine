module Sim
  ( initNetwork
  , stepNetwork
  , computeMD
  , writeMetricsHeader
  , appendMetrics
  , runCycles
  ) where

import Types
import System.IO (appendFile, writeFile)
import Control.Concurrent (threadDelay)
import Text.Printf (printf)
import System.Directory (createDirectoryIfMissing)

-- Build a simple deterministic network of n nodes.
-- Each node has a small vector state seeded from its id.
initNetwork :: Int -> InfoNetwork
initNetwork n = (nodes, edges)
  where
    nodes = [ INode i (seedVector i n) | i <- [1..n] ]
    edges = [ ERel i j (1.0 / (fromIntegral (abs (i-j) + 1))) 
            | i <- [1..n], j <- [1..n], i /= j ]

-- Simple seed for state vector (deterministic, no RNG to keep dependencies minimal)
seedVector :: Int -> Int -> [Double]
seedVector i n = [ base * (fromIntegral k) | k <- [1..4] ]
  where base = (fromIntegral i) / (fromIntegral n)

-- One update step: apply a tiny dynamics rule to each node's state.
-- This is intentionally simple: linear mixing + nonlinearity.
stepNetwork :: InfoNetwork -> InfoNetwork
stepNetwork (nodes, edges) = (map updateNode nodes, edges)
  where
    -- example: new state = tanh( old + mean(neighbor influence) )
    updateNode nd = nd { state = map (tanh . (+ bias)) (state nd) }
      where
        bias = neighborInfluence (nodeId nd) nodes edges

neighborInfluence :: NodeId -> [INode] -> [ERel] -> Double
neighborInfluence nid nodes edges =
  if null incoming then 0 else sum incomingStrengths / fromIntegral (length incoming)
  where
    incoming = [ e | e <- edges, toNode e == nid ]
    incomingStrengths = [ strength e * averageState (findNode (fromNode e) nodes) | e <- incoming ]

findNode :: NodeId -> [INode] -> INode
findNode i ns = case filter (\x -> nodeId x == i) ns of
  (x:_) -> x
  []    -> INode i (replicate 4 0.0)

averageState :: INode -> Double
averageState nd = let s = state nd in (sum s) / fromIntegral (length s)

-- Compute a simple "purity" per node: mean square of state elements.
-- MD (meaning-density analogue) = average purity across nodes.
computeMD :: InfoNetwork -> Double
computeMD (nodes, _) =
  let purities = map nodePurity nodes
  in if null purities then 0 else sum purities / fromIntegral (length purities)
  where
    nodePurity :: INode -> Double
    nodePurity nd = let s = state nd; ss = sum (map (\x -> x*x) s) in ss / fromIntegral (length s)

-- CSV helpers
writeMetricsHeader :: FilePath -> IO ()
writeMetricsHeader filepath = do
  createDirectoryIfMissing True "data"
  writeFile filepath "cycle,numNodes,numErels,avgMD\n"

appendMetrics :: FilePath -> Int -> InfoNetwork -> IO ()
appendMetrics filepath cycleNum (nodes, edges) = do
  let nNodes = length nodes
      nErels = length edges
      mdVal   = computeMD (nodes, edges)
      line = printf "%d,%d,%d,%.8f\n" cycleNum nNodes nErels mdVal
  appendFile filepath line

-- Run N cycles, appending metrics each cycle. Sleeps briefly between cycles.
runCycles :: Int -> FilePath -> InfoNetwork -> IO ()
runCycles total filepath net0 = go 1 net0
  where
    go c net
      | c > total = return ()
      | otherwise = do
          appendMetrics filepath c net
          -- small pause so logs are readable if watched (200ms)
          threadDelay (200 * 1000)
          let net' = stepNetwork net
          go (c + 1) net'
