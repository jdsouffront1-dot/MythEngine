module Metrics (meaningDensity, phiValue, coherenceMetric) where

meaningDensity :: Double -> Double -> Double
meaningDensity n e = (n * e) / (n + e + 1e-6)

phiValue :: Double -> Double -> Double
phiValue md entropy = md * logBase 2 (entropy + 1.1)

coherenceValue :: Double -> Double -> Double
coherenceValue phi md = sqrt (phi * md) / (1 + abs (phi - md))
-- Basic coherence metric (placeholder)
coherenceMetric :: [Double] -> Double
coherenceMetric xs =
  let n = fromIntegral (length xs)
      mean = sum xs / n
  in sqrt (sum [(x - mean)^2 | x <- xs] / n)


