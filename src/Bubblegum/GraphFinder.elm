module Bubblegum.GraphFinder exposing(..)

{-| This library provides a directed graph model for representing relationships between UI components.

# Query
@docs findNode, findEdgesBySource, findEdgesByDestination, findRootNodes, findLeafNodes, findConvergenceNodeIds, findMajorNodes, findNodeRole

-}
import List
import Set exposing (Set)
import Maybe
import Dict exposing (Dict)
import Bubblegum.GraphBuilder exposing (Graph, Node, Edge, createEdge)

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

type Irrelevant = Irrelevant

type alias MajorNodes = {
  root: Set String
  , leaf: Set String
  , convergence: Set String
}

type NodeRole =
  RootNode
  | ConvergenceNode
  | LeafNode
  | SimpleNode
  | NoNode

type alias NodeMeta = Node NodeRole
  
type alias EdgeMeta = Edge Irrelevant

type alias GraphIndex = {
  nodes: Dict String NodeMeta
  , sourceEdges: Dict String EdgeMeta
  , destEdges: Dict String EdgeMeta
}

noNode = {id= "!!!no-node!!!", role = NoNode}

toGraphIndex: Graph nData eData -> MajorNodes -> GraphIndex
toGraphIndex graph majorNodes =
  let
    nodes = graph.nodes |> List.map (\n-> (n.id, findNodeRole majorNodes n.id)) |> Dict.fromList
    sourceEdges = graph.edges |> List.map (\e -> (e.source, {e | value = Irrelevant})) |> Dict.fromList
  in
    {
      nodes = nodes
      , sourceEdges = sourceEdges
      , destEdges = sourceEdges --TODO
    }



{-| find node model.
-}
findNodeMeta: GraphIndex -> String -> NodeMeta
findNodeMeta graph id =
  graph.nodes |> Dict.get |> Maybe.withDefault noNode

{-| find edge models by source.
-}
findSourceMeta: GraphIndex -> String -> List EdgeMeta
findSourceMeta graph src =
  graph.sourceEdges |> Dict.get

{-| find edge models by destination.
-}
findDestinationMeta: GraphIndex -> String -> List EdgeMeta
findDestinationMeta graph dest =
  graph.destEdges |> Dict.get

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
  if Set.member nodeId majorNodes.root then RootNode
  else if Set.member nodeId majorNodes.leaf then LeafNode
  else if Set.member nodeId majorNodes.convergence then ConvergenceNode
  else SimpleNode 
 
-- findThisOrParent
findMajorParent: Graph nData eData -> Dict String NodeMeta -> String ->  NodeMeta
findMajorParent graph nodes nodeId =
  let
     nodeMeta = nodes.get nodeId |> Maybe.withDefault noNode
  in
    if nodeMeta.role == SimpleNode then
      findEdgesByDestination graph nodeId |> List.map .source |> List.head |> Maybe.map (findMajorParent graph nodes) |> Maybe.withDefault NoNode
    else
       nodeMeta      
      
{-| find the major parent
  we assume that we are not a root node
-}
findMajorParents: Graph nData eData -> MajorNodes -> String ->  List NodeMeta
findMajorParents graph majorNodes nodeId =
  findEdgesByDestination graph nodeId |> List.map .source |> List.map (findMajorParent graph majorNodes)

uniqueStringList: List String -> List String
uniqueStringList list =
  Set.fromList list |> Set.toList 

pairMajorParentAndChild: Graph nData eData -> MajorNodes -> String ->  List (Edge Irrelevant)
pairMajorParentAndChild graph majorNodes nodeId =
  findMajorParents graph majorNodes nodeId |> List.map nodeRoleToId |> uniqueStringList |> List.map (\pId -> createEdge "" pId nodeId Irrelevant )

nodeIdToNode: String -> Node Irrelevant
nodeIdToNode id = 
  {id= id, value = Irrelevant}

-- type MajorNode = MajorNode String (List MajorNode)

{-| find the major tree
  we assume that we are not a root node
-}
findMajorGraph: Graph nData eData -> MajorNodes -> Graph Irrelevant Irrelevant
findMajorGraph graph majorNodes =
  let
      majorParents = pairMajorParentAndChild graph majorNodes
      edges = majorNodes.convergence |> Set.toList |> List.map majorParents |> List.concat
      rootNodes = majorNodes.root |> Set.toList |> List.map nodeIdToNode
      convergenceNodes = majorNodes.convergence |> Set.toList |> List.map nodeIdToNode
  in
      {
        nodes = rootNodes ++ convergenceNodes
        , edges = edges
      }


