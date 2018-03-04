module Bubblegum.NodeRole exposing(..)

{-| This library provides a directed graph model for representing relationships between UI components.

# Query
@docs findNode, findEdgesBySource, findEdgesByDestination, findRootNodes, findLeafNodes, findConvergenceNodeIds, findMajorNodes, findNodeRole

-}
import Set exposing (Set)
import Bubblegum.GraphBuilder exposing (Graph)
import Bubblegum.GraphFinder exposing (findEdgesByDestination, findEdgesBySource)

type NodeRole =
  RootNode
  | ConvergenceNode
  | LeafNode
  | SimpleNode
  | NoNode

type alias MajorNodes = {
  root: Set String
  , leaf: Set String
  , convergence: Set String
  , major: Set String
}
 

 {-| find all the root nodes for the graph
    A root node should have not parent but at least one children
-}
findRootNodeIds: Graph nData eData -> Set String
findRootNodeIds graph =
  let
      findParents = findEdgesByDestination graph
      findChildren = findEdgesBySource graph
  in
      graph.nodes 
        |> List.filter (\node -> findParents node.id |> List.isEmpty ) 
        |> List.filter (\node -> findChildren node.id |> List.isEmpty |> not)
        |> List.map .id 
        |> Set.fromList

{-| find all the leaf nodes for the graph
    A leaf node should have at least a parent but no children
-}
findLeafNodeIds: Graph nData eData -> Set String
findLeafNodeIds graph =
  let
      findParents = findEdgesByDestination graph
      findChildren = findEdgesBySource graph
  in
      graph.nodes 
        |> List.filter (\node -> findChildren node.id |> List.isEmpty)         
        |> List.filter (\node -> findParents node.id |> List.isEmpty |> not ) 
        |> List.map .id 
        |> Set.fromList

greaterThanOne: List a -> Bool
greaterThanOne list =
  List.length list > 1

{-| find all the convergence nodes for the graph
    A convergence node should have multiple parents and at least one child
-}
findConvergenceNodeIds: Graph nData eData -> Set String
findConvergenceNodeIds graph =
  let
      findParents = findEdgesByDestination graph
      findChildren = findEdgesBySource graph
  in
      graph.nodes 
        |> List.filter (\node -> findParents node.id |> greaterThanOne )         
        |> List.filter (\node -> findChildren node.id |> List.isEmpty |> not )
        |> List.map .id 
        |> Set.fromList


 {-| find all the major nodes for the graph
-}
findMajorNodes: Graph nData eData -> MajorNodes
findMajorNodes graph =
  let
      root = findRootNodeIds(graph)
      leaf = findLeafNodeIds(graph)
      convergence = findConvergenceNodeIds(graph)
      major = Set.union root convergence
  in
      
  {
  root = root 
  , leaf = leaf
  , convergence = convergence
  , major = major
  }  

{-| find the role of a node
-}
findNodeRole: MajorNodes -> String -> NodeRole
findNodeRole majorNodes nodeId =
  if Set.member nodeId majorNodes.root then RootNode
  else if Set.member nodeId majorNodes.leaf then LeafNode
  else if Set.member nodeId majorNodes.convergence then ConvergenceNode
  else SimpleNode 
     