module Bubblegum.Edge exposing(Edge, toTuple, fromTuple, create)

{-| This library provides a directed graph model for representing relationships between UI components.

# Build
@docs createNode, createEdge, createGraph

-}
import Tuple exposing (first, second)

{-| The core representation of a value.
-}
type alias Edge eData= {
    source: String
    , destination: String
    , value: eData
 }

{-| Creates a edge.
-}
create: String -> String -> eData -> Edge eData
create source destination edgeData=
  {
    source = source
    , destination = destination
    , value = edgeData
 }

toTuple: Edge eData -> ((String, String), Edge eData)
toTuple edge =
  ((edge.source, edge.destination), edge)

fromTuple: ((String, String), Edge eData) -> Edge eData
fromTuple tuple =
  second(tuple)

