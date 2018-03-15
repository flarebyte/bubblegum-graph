module Bubblegum.Relations exposing(Relations, create)

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

