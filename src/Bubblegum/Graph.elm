module Bubblegum.Graph exposing(createNode, createEdge, createGraph)

{-| This library provides a directed graph model for representing relationships between UI components.

# Construction of graph
@docs createNode, createEdge, createGraph

-}
import List
import Set exposing (Set)
import Maybe


{-| The core representation of a value.
-}
type alias EdgeModel eData= {
    id: String
    , source: String
    , destination: String
    , value: eData
 }

type alias NodeModel nData= {
    id: String
    , value: nData
 }

{-| The core representation of a value.
-}
type alias GraphModel nData eData = {
    nodes: List  (NodeModel nData)
    , edges: List (EdgeModel eData)
  }

{-| Creates a node.
-}
createNode: String -> nData -> NodeModel nData
createNode id nodeData=
  {
    id = id
    , value = nodeData
 }

{-| Creates a edge.
-}
createEdge: String -> String -> String -> eData -> EdgeModel eData
createEdge id source destination edgeData=
  {
    id = id
    , source = source
    , destination = destination
    , value = edgeData
 }

{-| Render.
-}
createGraph: List  (NodeModel nData) -> List (EdgeModel eData) -> GraphModel nData eData
createGraph nodes edges=
  {
    nodes = nodes
    , edges = edges
  }
