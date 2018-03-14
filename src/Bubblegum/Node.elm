module Bubblegum.Node exposing(Node, create, toTuple, fromTuple)

{-| This library provides a directed graph model for representing relationships between UI components.

# Build
@docs createNode, createEdge, createGraph

-}

import Tuple exposing(first, second)

{-| The core representation of a value.
-}
type alias Node nData= {
    id: String
    , value: nData
 }

{-| Creates a node.
-}
create: String -> nData -> Node nData
create id nodeData=
  {
    id = id
    , value = nodeData
 }

toTuple: Node nData -> (String, Node nData)
toTuple node =
  (node.id, node)

fromTuple: (String, Node nData) -> Node nData
fromTuple tuple =
  {
    id= first(tuple)
    , value = second(tuple) |> .value
  }
