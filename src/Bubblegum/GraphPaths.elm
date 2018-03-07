module Bubblegum.GraphPaths exposing(Path, GraphPaths, createGraphPaths, byId, byNodeIds, parent, descendants, descendantsOrSelf, children)

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

byId: GraphPaths -> Int -> Maybe Path
byId paths id =
  Dict.get id paths.idToPath


matchNodeIds: List String -> Path -> Bool
matchNodeIds nodeIds path = path.nodeIds == nodeIds


byNodeIds: GraphPaths -> List String -> Maybe Path
byNodeIds paths nodeIds =
  paths.paths |> List.filter (matchNodeIds nodeIds) |> List.head

{-
  Selects the parent if any
-}
parent: GraphPaths -> Path -> Maybe Path
parent paths path=
  path.nodeIds |> List.tail |> Maybe.andThen (byNodeIds paths)

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

matchChildren: List String -> Path -> Bool
matchChildren nodeIds path =
 let
     sizeOfParentPath = List.length nodeIds
     sizeOfVisitedPath = List.length path.nodeIds
     diffSize = sizeOfVisitedPath - sizeOfParentPath
     parentOfVisited = List.drop diffSize path.nodeIds
 in
    parentOfVisited == nodeIds && sizeOfVisitedPath == (sizeOfParentPath + 1)


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
children: GraphPaths -> Path -> List Path
children paths path=
  List.filter (matchChildren path.nodeIds) paths.paths



