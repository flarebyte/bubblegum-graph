module Bubblegum.GraphIndex exposing(..)

{-| This library provides a directed graph model for representing relationships between UI components.

# Query
@docs findNode, findEdgesBySource, findEdgesByDestination, findRootNodes, findLeafNodes, findConvergenceNodeIds, findMajorNodes, findNodeRole

-}
import List
import Set exposing (Set)
import Maybe
import Dict exposing (Dict)
import Bubblegum.GraphBuilder exposing (Graph, Node, Edge, createEdge)
import Bubblegum.NodeRole exposing(NodeRole(..))

type Irrelevant = Irrelevant

type alias NodeMeta = Node NodeRole
  
type alias EdgeMeta = Edge Irrelevant

type alias GraphMeta = Graph NodeRole Irrelevant

type alias GraphIndex = {
  nodes: Dict String NodeMeta
  , sourceEdges: Dict String (List EdgeMeta)
  , destEdges: Dict String (List EdgeMeta)
  , majorNodes: MajorNodes
}

noNode = {id= "!!!no-node!!!", value = NoNode}


{-| Takes a key-fn and a list.
  Creates a `Dict` which maps the key to a list of matching elements.
    groupBy String.length [ "tree" , "apple" , "leaf" ]
    --> Dict.fromList [ ( 4, [ "tree", "leaf" ] ), ( 5, [ "apple" ] ) ]
  Code borrowed from: https://github.com/elm-community/dict-extra/blob/master/src/Dict/Extra.elm
-}
groupBy : (a -> comparable) -> List a -> Dict comparable (List a)
groupBy keyfn list =
    List.foldr
        (\x acc ->
            Dict.update (keyfn x) (Maybe.map ((::) x) >> Maybe.withDefault [ x ] >> Just) acc
        )
        Dict.empty
        list

toGraphIndex: Graph nData eData -> MajorNodes -> GraphIndex
toGraphIndex graph majorNodes =
  let
    nodes = graph.nodes |> List.map (\n-> (n.id, {id = n.id, value = findNodeRole majorNodes n.id})) |> Dict.fromList
    sourceEdges = graph.edges |> List.map (\e -> {e | value = Irrelevant}) |> groupBy .source
    destEdges = graph.edges |> List.map (\e -> {e | value = Irrelevant}) |> groupBy .destination
  in
    {
      nodes = nodes
      , sourceEdges = sourceEdges
      , destEdges = destEdges
      , majorNodes = majorNodes
    }



{-| find node model.
-}
findNodeMeta: GraphIndex -> String -> NodeMeta
findNodeMeta graph id =
  Dict.get id graph.nodes |> Maybe.withDefault noNode

{-| find edge models by source.
-}
findSourceMeta: GraphIndex -> String -> List EdgeMeta
findSourceMeta graph src =
  Dict.get src graph.sourceEdges |> Maybe.withDefault []

{-| find edge models by destination.
-}
findDestinationMeta: GraphIndex -> String -> List EdgeMeta
findDestinationMeta graph dest =
  Dict.get  dest graph.destEdges |> Maybe.withDefault []

 
-- findThisOrParent single parent
findMajorParent: GraphIndex -> String ->  NodeMeta
findMajorParent graph nodeId =
  let
     nodeMeta = findNodeMeta graph nodeId
  in
    if nodeMeta.value == SimpleNode then
      -- get the first parent and recurse
      findDestinationMeta graph nodeId |> List.head |>  Maybe.map (\e -> findMajorParent graph e.source) |> Maybe.withDefault noNode
    else
       nodeMeta      
      
{-| find the major parent
  we assume that we are not a root node
-}
findMajorParents: GraphIndex -> String ->  List NodeMeta
findMajorParents graph nodeId =
  findDestinationMeta graph nodeId |> List.map .source |> List.map (findMajorParent graph)

findMajorEdges: GraphIndex -> String ->  List EdgeMeta
findMajorEdges graph nodeId =
  findMajorParents graph nodeId |> List.map (\n -> {id= "", source = n.id, destination = nodeId, value = Irrelevant})
