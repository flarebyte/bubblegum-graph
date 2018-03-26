module Bubblegum.GraphPaths exposing(create, GraphPaths, byId, byNodeIds, parent, descendants, descendantsOrSelf, children)

{-| GraphPaths helps a graph as a list of paths.

# Build
@docs create

#Query
@docs byId, byNodeIds, parent, descendants, descendantsOrSelf, children

-}

import Dict exposing (Dict)
import Bubblegum.Path as Path exposing(..)

type alias GraphPaths = {
  paths: List Path
  , idToPath: Dict String Path
}

{-| Creates an object representing the paths withn a graph -}
create: List Path -> GraphPaths
create paths =
  let
        idToPath = paths |> List.map pathAsTuple |> Dict.fromList
  in
    {
      paths = paths
      , idToPath = idToPath
    }

{-| Retrieve a path by id 

  byid graphPaths "001/011/111"
-}
byId: GraphPaths -> String -> Maybe Path
byId paths id =
  Dict.get id paths.idToPath


{-| Retrieve a path by providing a list of nodeIds 

  byNodeIds graphPaths ["Athena", "Zeus"]
-}
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

-- FOR INTERNAL USE ONLY
-- Private methods

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

pathAsTuple: Path -> (String, Path)
pathAsTuple path =
  (path.id, path)

matchNodeIds: List String -> Path -> Bool
matchNodeIds nodeIds path = path.nodeIds == nodeIds
