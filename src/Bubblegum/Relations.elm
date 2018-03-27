module Bubblegum.Relations exposing(create, Relations, isLeaf, isRoot)

{-| Relations helps to describe the inbound and outbound relationships to a node.

# Build
@docs create, Relations

# Test
@docs isLeaf, isRoot

-}

{-| The representation of the inbound and outbound relationships to a node.
  
    { inbound = [("London", "Paris"), ("Amsterdam", "Paris")], outbound = [("Paris", "New York")] }

-}
type alias Relations = {
    inbound: List (String, String)
    , outbound: List (String, String) 
 }


{-| Create the relations against a node providing the inbound and outbound values.

    create [("London", "Paris"), ("Amsterdam", "Paris")] [("Paris", "New York")]
 
-}
create: List (String, String) -> List (String, String) -> Relations
create inbound outbound =
  {
    inbound = inbound
    , outbound = outbound
 }

{-| Test whether the relations represents a leaf in the graph. No more children.

    isLeaf { inbound = [("London", "Paris"), ("Amsterdam", "Paris")], outbound = [] } == True

-}
isLeaf: Relations -> Bool
isLeaf relation =
  List.isEmpty relation.outbound

{-| Test whether the relations represents a root in the graph. No more parents.

    isRoot { inbound = [], outbound = [("Paris", "New York")] } == True

-}
isRoot: Relations -> Bool
isRoot relation =
  List.isEmpty relation.inbound