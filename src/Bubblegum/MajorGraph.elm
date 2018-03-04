module Bubblegum.MajorGraph exposing(toMajorGraph)

{-| This library provides a directed graph model for representing relationships between UI components.

# Query
@docs findNode, findEdgesBySource, findEdgesByDestination, findRootNodes, findLeafNodes, findConvergenceNodeIds, findMajorNodes, findNodeRole

-}
import List
import Set exposing (Set)
import Tuple
import Bubblegum.GraphBuilder exposing (Graph, Node, Edge, Irrelevant(..), createEdge)
import Bubblegum.GraphIndex exposing (GraphIndex, EdgeMeta, findNodeMeta, findMajorParents)
import Bubblegum.NodeRole exposing (NodeRole)

type alias GraphMeta = Graph NodeRole Irrelevant

edgeFromTuple: (String, String) -> Edge Irrelevant
edgeFromTuple edge = 
  {id= "", source = Tuple.first(edge), destination = Tuple.second(edge), value = Irrelevant}

uniqTupleList: List (String, String) -> List (String, String)
uniqTupleList list =
  list |> Set.fromList |> Set.toList

findMajorEdges: GraphIndex -> String ->  List EdgeMeta
findMajorEdges graph nodeId =
  findMajorParents graph nodeId |> List.map (\n -> (n.id, nodeId)) |> uniqTupleList |> List.map edgeFromTuple

{-| find the major tree
  we assume that we are not a root node
-}
toMajorGraph: GraphIndex -> GraphMeta
toMajorGraph graph =
  let
      nodes = graph.majorNodes.major |> Set.toList |> List.map (findNodeMeta graph)
      edges = graph.majorNodes.convergence |> Set.toList |> List.map (findMajorEdges graph) |> List.concat
  in
      {
        nodes = nodes
        , edges = edges
      }


