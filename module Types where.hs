module Types where

type NodeId = Int

-- A node carries a small numeric state vector (simple, interpretable)
data INode = INode
  { nodeId :: NodeId
  , state  :: [Double]
  } deriving (Show, Eq)

-- Edge / relation between nodes (directed)
data ERel = ERel
  { fromNode :: NodeId
  , toNode   :: NodeId
  , strength :: Double
  } deriving (Show, Eq)

type InfoNetwork = ([INode], [ERel])
