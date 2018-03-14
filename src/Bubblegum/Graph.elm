module Bubblegum.Graph exposing(Graph, create, findNode, toNodeList, findEdgesBySource, findEdgesByDestination)

{-| This library provides a directed graph model for representing relationships between UI components.

# Build
@docs createNode, createEdge, createGraph

-}

import Dict exposing(Dict)
import Bubblegum.Node as Node exposing(..)
import Bubblegum.Edge as Edge exposing(..)


{-| The core representation of a value.
-}
type alias Graph nData eData = {
    nodes: Dict String (Node nData)
    , edges: List (Edge eData)
  }

{-| Create graph.
-}
create: List  (Node nData) -> List (Edge eData) -> Graph nData eData
create nodes edges=
  {
    nodes = nodes |> List.map Node.toTuple |> Dict.fromList
    , edges = edges
  }

{-| find node model.
-}
findNode: Graph nData eData -> String -> Maybe (Node nData)
findNode graph id =
  Dict.get id graph.nodes

toNodeList: Graph nData eData -> List  (Node nData)
toNodeList graph =
  Dict.values graph.nodes

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



