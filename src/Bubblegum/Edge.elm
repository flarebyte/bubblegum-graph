module Bubblegum.Edge exposing(Edge, toTuple, create)

{-| This library provides a directed graph model for representing relationships between UI components.

# Build
@docs createNode, createEdge, createGraph

-}

{-| The core representation of a value.
-}
type alias Edge eData= {
    source: String
    , destination: String
    , value: eData
 }

toTuple: Edge eData -> (String, String)
toTuple edge =
  (edge.source, edge.destination)

{-| Creates a edge.
-}
create: String -> String -> eData -> Edge eData
create source destination edgeData=
  {
    source = source
    , destination = destination
    , value = edgeData
 }

