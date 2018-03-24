module Bubblegum.Node exposing(create, Node, toTuple, fromTuple)

{-| Node helps to represent the data about a node or vertex.

# Build
@docs create, Node

# Conversion
@docs toTuple, fromTuple

-}

import Tuple exposing(first, second)

{-| The core representation of a node or vertex.

  {id = "London", value = "8,787,892 inhabitants"}
-}
type alias Node nData= {
    id: String
    , value: nData
 }

{-| Creates a node or vertex providing the id and value.

  create "London" "8,787,892 inhabitants"
-}
create: String -> nData -> Node nData
create id nodeData=
  {
    id = id
    , value = nodeData
 }

{-| Convert a node to a tuple.

  toTuple {id = "London", value = "8,787,892 inhabitants"} == ("London", "8,787,892 inhabitants")

-}
toTuple: Node nData -> (String, Node nData)
toTuple node =
  (node.id, node)

{-| Convert a tuple to a node.

  fromTuple ("London", "8,787,892 inhabitants") == {id = "London", value = "8,787,892 inhabitants"}

-}
fromTuple: (String, Node nData) -> Node nData
fromTuple tuple =
  {
    id= first(tuple)
    , value = second(tuple) |> .value
  }
