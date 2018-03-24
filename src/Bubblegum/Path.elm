module Bubblegum.Path exposing(create, Path)

{-| Path helps to represent a path between two nodes.

# Build
@docs create, Path

-}
import Dict exposing(Dict)
import String exposing (padLeft)


{-| Representation of path in the Graph -}
type alias Path = {
  id: String
  , nodeIds: List String -- tail is parent, left to right == children to parent
}

{-| Create a path providing an unique id and a list of node ids, children first 

  create "0012/001" ["Athena", "Zeus"]
-}
create: Dict String Int -> List String -> Path
create idToInt nodeIds =
  {
  id = id
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
