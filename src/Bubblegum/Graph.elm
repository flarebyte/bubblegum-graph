module Bubblegum.Graph exposing(Graph, create)

{-| This library provides a directed graph model for representing relationships between UI components.

# Build
@docs createNode, createEdge, createGraph

-}

import Bubblegum.Node exposing(..)
import Bubblegum.Edge exposing(..)


{-| The core representation of a value.
-}
type alias Graph nData eData = {
    nodes: List  (Node nData)
    , edges: List (Edge eData)
  }

{-| Create graph.
-}
create: List  (Node nData) -> List (Edge eData) -> Graph nData eData
create nodes edges=
  {
    nodes = nodes
    , edges = edges
  }


