module Bubblegum.Relations exposing(Relations, create, isLeaf, isRoot)

{-| This library provides a directed graph model for representing relationships between UI components.

# Build
@docs createNode, createEdge, createGraph

-}

{-| The core representation of a value.
-}
type alias Relations = {
    inbound: List (String, String)
    , outbound: List (String, String) 
 }


{-| Creates a edge.
-}
create: List (String, String) -> List (String, String) -> Relations
create inbound outbound =
  {
    inbound = inbound
    , outbound = outbound
 }

size: Relations -> (Int, Int)
size relation =
  (List.length relation.inbound, List.length relation.outbound)

isLeaf: Relations -> Bool
isLeaf relation =
  List.isEmpty relation.outbound

isRoot: Relations -> Bool
isRoot relation =
  List.isEmpty relation.inbound  