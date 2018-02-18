module Bubblegum.Graph exposing(getPathList)

{-| This library provides a directed graph model for representing relationships between UI components.

# Extract paths
@docs getPathList

-}
import Bubblegum.GraphBuilder exposing (Graph)


{-| The core representation of a value.
-}
type alias Path = {
    edgeIds: List String
  }


{-| find path models.
-}
getPathList: Graph nData eData -> String -> List Path
getPathList graph rootNode =
  []
