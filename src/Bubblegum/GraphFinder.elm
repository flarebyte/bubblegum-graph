module Bubblegum.GraphFinder exposing(..)

{-| This library provides a directed graph model for representing relationships between UI components.

# Query
@docs findNode, findEdgesBySource, findEdgesByDestination, findRootNodes, findLeafNodes, findConvergenceNodeIds, findMajorNodes, findNodeRole

-}
import List
import Set exposing (Set)
import Maybe
import Bubblegum.GraphBuilder exposing (Graph, Node, Edge)

{-| find node model.
-}
findNode: Graph nData eData -> String -> Maybe (Node nData)
findNode graph id =
  graph.nodes |> List.filter (\n -> n.id == id)|> List.head

{-| find edge models by source.
-}
findEdgesBySource: Graph nData eData -> String -> List (Edge eData)
findEdgesBySource graph src =
  graph.edges |> List.filter (\edge -> edge.source == src)

{-| find edge models by destination.
-}
findEdgesByDestination: Graph nData eData -> String -> List (Edge eData)
findEdgesByDestination graph dest =
  graph.edges |> List.filter (\edge -> edge.destination == dest)

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

type alias MajorNodes = {
  root: Set String
  , leaf: Set String
  , convergence: Set String
}

type NodeRole =
  RootNode String
  | ConvergenceNode String
  | LeafNode String
  | SimpleNode String
  | NoNode

{-| find all the major nodes for the graph
-}
findMajorNodes: Graph nData eData -> MajorNodes
findMajorNodes graph =
  {
  root =  findRootNodeIds(graph)
  , leaf = findLeafNodeIds(graph)
  , convergence = findConvergenceNodeIds(graph)
 }

{-| find the role of a node
-}
findNodeRole: MajorNodes -> String -> NodeRole
findNodeRole majorNodes nodeId =
  if Set.member nodeId majorNodes.root then RootNode nodeId
  else if Set.member nodeId majorNodes.leaf then LeafNode nodeId
  else if Set.member nodeId majorNodes.convergence then ConvergenceNode nodeId
  else SimpleNode nodeId 
 

findMajorParent: Graph nData eData -> MajorNodes -> String ->  NodeRole
findMajorParent graph majorNodes nodeId =
  let
     role = findNodeRole majorNodes nodeId
  in
    case role of
      RootNode id -> 
        RootNode id
      ConvergenceNode id ->
       ConvergenceNode id
      LeafNode id->
        LeafNode id -- should not happen !
      NoNode ->
        NoNode
      SimpleNode id ->
        findEdgesByDestination graph id |> List.map .source |> List.head |> Maybe.map (findMajorParent graph majorNodes) |> Maybe.withDefault NoNode
      
  

{-| find the major parent
  we assume that we are not a root node
-}
findMajorParents: Graph nData eData -> MajorNodes -> String ->  List NodeRole
findMajorParents graph majorNodes nodeId =
  findEdgesByDestination graph nodeId |> List.map .source |> List.map (findMajorParent graph majorNodes)

type MajorNode = MajorNode String (List MajorNode)

{-| find the major tree
  we assume that we are not a root node
-}
findMajorTree: Graph nData eData -> MajorNodes -> MajorNode
findMajorTree graph majorNodes =
  let
      majorParents = findMajorParents graph majorNodes
      convergenceNodes = majorNodes.convergence |> Set.toList |> List.map majorParents
      -- convert node -> parent to parent -> node
  in
      MajorNode "" []


