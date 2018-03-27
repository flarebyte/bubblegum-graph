module Bubblegum.Path exposing(create, Path)

{-| Path helps to represent a path between two nodes.

# Build
@docs create, Path

-}
import Dict exposing(Dict)
import String exposing (padLeft)
import List as List
import Maybe as Maybe


{-| Representation of path in the Graph.

    { id = "002/001", nodeIds = ["Athena", "Zeus"] }

 * id is a unique id representing this path in the graph. The expected format is a list of integers seperated by slashes. Each integer represents a nodeIds sorted from parents to children.
 * nodeIds represent a list of nodeIds, sorted from children to parent.

 -}
type alias Path = {
  id: String
  , nodeIds: List String -- tail is parent, left to right == children to parent
}

{-| Create a path providing an unique id and a list of node ids, children first. 

    create (Dict.fromList [("Athena", 1), ("Zeus", 2)]  ["Athena", "Zeus"]
-}
create: Dict String Int -> List String -> Path
create idToInt nodeIds =
  let
      nodeIdToInt id = Dict.get id idToInt |> Maybe.withDefault -1
  in
   {
    id = List.map nodeIdToInt nodeIds |> joinInt
    , nodeIds = nodeIds
   }

-- FOR INTERNAL USE ONLY
-- Private methods

-- Pads numbers with leading zero.
-- padInt 7 == 007
padInt: Int -> String
padInt int =
  padLeft 3 '0' (toString int)

-- join a list of integer and reverse
-- joinInt [1, 2] == "2/1"
joinInt: List Int -> String
joinInt list = 
  list |> List.reverse |> List.map padInt |> String.join "/"
