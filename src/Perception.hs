module Perception where

-- The scaling factor (kappa) translates the dimensionless internal
-- efficiency (epsilon) into a physical, observable gravitational wave
-- strain (epsilon_GW).
kappa :: Double
kappa = 1e-4

-- | Calculates the observable Gravitational Wave (GW) strain.
-- | epsilon_GW = epsilon * kappa
calculateEpsilonGW :: Double -> Double
calculateEpsilonGW epsilon = epsilon * kappa
