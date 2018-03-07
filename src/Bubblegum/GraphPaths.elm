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
   athena, zeus

-}
matchDescendantOrSelf: List String -> Path -> Bool
matchDescendantOrSelf nodeIds path =
 let
     sizeOfParentPath = List.length nodeIds
     sizeOfVisitedPath = List.length path.nodeIds
     diffSize = sizeOfVisitedPath - sizeOfParentPath
     parentOfVisited = List.drop diffSize path.nodeIds
 in
    parentOfVisited == nodeIds

matchDescendant: List String -> Path -> Bool
matchDescendant nodeIds path =
 let
     sizeOfParentPath = List.length nodeIds
     sizeOfVisitedPath = List.length path.nodeIds
     diffSize = sizeOfVisitedPath - sizeOfParentPath
     parentOfVisited = List.drop diffSize path.nodeIds
 in
    parentOfVisited == nodeIds && sizeOfParentPath /= sizeOfVisitedPath

{-
  Selects all descendants
-}
descendants: GraphPaths -> Path -> List Path
descendants paths path=
  List.filter (matchDescendant path.nodeIds) paths.paths
{-
  Selects all descendants and itself
-}
descendantsOrSelf: GraphPaths -> Path -> List Path
descendantsOrSelf paths path=
  List.filter (matchDescendantOrSelf path.nodeIds) paths.paths

{-
  Selects all children
-}
child: GraphPaths -> Path -> List Path
child paths path=
  List.filter (matchDescendant path.nodeIds) paths.paths



