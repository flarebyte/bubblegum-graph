module Bubblegum.GraphBuilder exposing(createNode, createEdge, createGraph, Edge, Node, Graph, Irrelevant(..))

{-| This library provides a directed graph model for representing relationships between UI components.

# Build
@docs createNode, createEdge, createGraph

-}

{-| The core representation of a value.
-}
type alias Edge eData= {
    id: String
    , source: String
    , destination: String
    , value: eData
 }

{-| The core representation of a value.
-}
type alias Node nData= {
    id: String
    , value: nData
 }


{-| The core representation of a value.
-}
type alias Graph nData eData = {
    nodes: List  (Node nData)
    , edges: List (Edge eData)
  }

type Irrelevant = Irrelevant

{-| Creates a node.
-}
createNode: String -> nData -> Node nData
createNode id nodeData=
  {
    id = id
    , value = nodeData
 }

{-| Creates a edge.
-}
createEdge: String -> String -> String -> eData -> Edge eData
createEdge id source destination edgeData=
  {
    id = id
    , source = source
    , destination = destination
    , value = edgeData
 }

{-| Create graph.
-}
createGraph: List  (Node nData) -> List (Edge eData) -> Graph nData eData
createGraph nodes edges=
  {
    nodes = nodes
    , edges = edges
  }

edgeAsTuple: Edge eData -> (String, String)
edgeAsTuple edge =
  (edge.source, edge.destination)

