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

pathAsTuple: Path -> (Int, Path)
pathAsTuple path =
  (path.id, path)

createGraphPaths: List Path -> GraphPaths
createGraphPaths paths =
  let
        idToPath = paths |> List.map pathAsTuple |> Dict.fromList
  in
    {
      paths = paths
      , idToPath = idToPath
    }

findPathById: GraphPaths -> Int -> Maybe Path
findPathById paths id =
  Dict.get id paths.idToPath


matchNodeIds: List String -> Path -> Bool
matchNodeIds nodeIds path = path.nodeIds == nodeIds


findPathByNodeIds: GraphPaths -> List String -> Maybe Path
findPathByNodeIds paths nodeIds =
  paths.paths |> List.filter (matchNodeIds nodeIds) |> List.head

{-
  Selects the parent if any
-}
parent: GraphPaths -> Path -> Maybe Path
parent paths path=
  path.nodeIds |> List.tail |> Maybe.andThen (findPathByNodeIds paths)


{-
  Selects the grand parent if any
-}
grandParent: GraphPaths -> Path -> Maybe Path
grandParent paths path=
  path.nodeIds |> List.tail |> Maybe.andThen (findPathByNodeIds paths)

{-
   athena, zeus

-}
matchDescendant: List String -> Path -> Bool
matchDescendant nodeIds path =
 let
     sizeOfParentPath = List.length nodeIds
     sizeOfVisitedPath = List.length path.nodeIds
     diffSize = sizeOfVisitedPath - sizeOfParentPath
     parentOfVisited = List.drop diffSize path.nodeIds
 in
    parentOfVisited == nodeIds

{-
  Selects all descendants
-}
descendant: GraphPaths -> Path -> List Path
descendant paths path=
  List.filter (matchDescendant path.nodeIds) paths.paths

{-
  Selects all descendants and itself
-}
descendantOrSelf: GraphPaths -> Path -> List Path
descendantOrSelf paths path=
  List.filter (matchDescendant path.nodeIds) paths.paths

{-
  Selects all children
-}
child: GraphPaths -> Path -> List Path
child paths path=
  List.filter (matchDescendant path.nodeIds) paths.paths



