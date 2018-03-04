module Bubblegum.GraphPaths exposing(..)

{-| This library provides a directed graph model for representing relationships between UI components.

# Build
@docs

-}

import Dict exposing (Dict)

{-| The core representation of a value.
-}
type alias Path = {
  id: Int
  , nodeIds: List String -- tail is parent, left to right == children to parent
}

type alias GraphPaths = {
  paths: List Path
  , idToPath: Dict Int Path
}

findPathById: GraphPaths -> Int -> Maybe Path
findPathById paths id =
  Dict.get id paths.idToPath


matchNodeIds: List String -> Path -> Bool
matchNodeIds nodeIds path = path.nodeIds == nodeIds


findPathByNodeIds: GraphPaths -> List String -> Maybe Path
findPathByNodeIds paths nodeIds =
  paths.paths |> List.filter (matchNodeIds nodeIds) |> List.head


parent: GraphPaths -> Path -> Maybe Path
parent paths path=
  path.nodeIds |> List.tail |> Maybe.andThen (findPathByNodeIds paths)

matchDescendant: List String -> Path -> Bool
matchDescendant nodeIds path =
 path.nodeIds == nodeIds --TODO filter by right

descendant: GraphPaths -> Path -> List Path
descendant paths path=
  List.filter (matchDescendant path.nodeIds) paths.paths




