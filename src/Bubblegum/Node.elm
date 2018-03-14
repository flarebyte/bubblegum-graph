module Bubblegum.Node exposing(Node, create)

{-| This library provides a directed graph model for representing relationships between UI components.

# Build
@docs createNode, createEdge, createGraph

-}

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


