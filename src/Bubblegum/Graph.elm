module Bubblegum.Graph exposing(Graph, create, findNode, findEdgesBySource, findEdgesByDestination)

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

{-| find node model.
  linear time O(n)
-}
findNode: Graph nData eData -> String -> Maybe (Node nData)
findNode graph id =
  graph.nodes |> List.filter (\n -> n.id == id)|> List.head

{-| find edge models by source.
  linear time O(n)
-}
findEdgesBySource: Graph nData eData -> String -> List (Edge eData)
findEdgesBySource graph src =
  graph.edges |> List.filter (\edge -> edge.source == src)

{-| find edge models by destination.
  linear time O(n)
-}
findEdgesByDestination: Graph nData eData -> String -> List (Edge eData)
findEdgesByDestination graph dest =
  graph.edges |> List.filter (\edge -> edge.destination == dest)



