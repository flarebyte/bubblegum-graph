module Bubblegum.GraphFinder exposing(findNode, findEdgesBySource, findEdgesByDestination)

{-| This library provides a directed graph model for representing relationships between UI components.

# Query
@docs

-}
import List
import Maybe
import Bubblegum.Node exposing(..)
import Bubblegum.Edge exposing(..)
import Bubblegum.Graph exposing (Graph)

{-| find node model.
  linear time O(n)
-}
findNode: Graph nData eData -> String -> Maybe (Node nData)
findNode graph id =
  graph.nodes |> List.filter (\n -> n.id == id)|> List.head

{-| find edge models by source.
  linear time O(n)
-}
findEdgesBySource: Graph nData eData -> String -> List (Edge eData)
findEdgesBySource graph src =
  graph.edges |> List.filter (\edge -> edge.source == src)

{-| find edge models by destination.
  linear time O(n)
-}
findEdgesByDestination: Graph nData eData -> String -> List (Edge eData)
findEdgesByDestination graph dest =
  graph.edges |> List.filter (\edge -> edge.destination == dest)



