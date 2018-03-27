module Bubblegum.Graph exposing(create, Graph, findNode, findEdge, findEdgesBySource, findEdgesByDestination, findRelations, toPaths)

{-| Graph helps to represent a directed acyclic graph consisting of a set of nodes and a set of edges.

This is an **experimental** implementation which attempts to expose a graph as a list of paths, similar in concept to xpath.
In other words, the graph is converted to a flat list to make processing easier.

This library has been implemented with the idea of of been used for the Bubblegum UI library. However, it should be possible to use it for other purposes.

If you are looking for a general purpose Graph library, I would recommend [the elm-community/graph one](https://github.com/elm-community/graph) instead.

Limits of Bubblegum.Graph:
 * No more than 1000 nodes or edges.
 * Only one edge with the same source and destination.
 * The graph should not have any cycles (loops).

# Build
@docs create, Graph

# Query
@docs findNode, findEdge, findEdgesBySource, findEdgesByDestination, findRelations, toPaths
-}

import Dict exposing(Dict)
import Set exposing(Set)
import Tuple exposing (first, second)
import Bubblegum.Irrelevant exposing(..)
import Bubblegum.Node as Node exposing(..)
import Bubblegum.Edge as Edge exposing(..)
import Bubblegum.Relations as Relations exposing(..)
import Bubblegum.Path as Path
import Bubblegum.GraphPaths as GraphPaths exposing(GraphPaths)

{-| The representation of a graph.

 * nodes and edges hold the main data model and are implemented as dictionaries to facilitate quick direct access.
 * relations is special view of the graph to explore parent/child relationships (inbound/outbound).
 * paths is a special view of the graph to expose the graph as a list of paths. 

-}
type alias Graph nData eData = {
    nodes: Dict String (Node nData)
    , edges: Dict (String, String) (Edge eData)
    , relations: Dict String Relations
    , paths: GraphPaths
  }

{-| Create graph providing a list of nodes and edges.
-}
create: List  (Node nData) -> List (Edge eData) -> Graph nData eData
create nodes edges=
  let
      relations = createRelations edges
  in
      
  {
    nodes = nodes |> List.map Node.toTuple |> Dict.fromList
    , edges = edges |> List.map Edge.toTuple |> Dict.fromList
    , relations = relations
    , paths = createGraphPaths relations
  }
  
{-| Find a node from the graph.

    findNode graph "Zeus"
-}
findNode: Graph nData eData -> String -> Maybe (Node nData)
findNode graph id =
  Dict.get id graph.nodes

{-| Find the edge by source and destination. There should be no more than one edge for same source and destination.

    findEdge graph ("Zeus", "Athena")
-}
findEdge: Graph nData eData -> (String, String) -> Maybe (Edge eData)
findEdge graph sourceDest =
   Dict.get sourceDest graph.edges 

{-| Find edge models by source.

    findEdgesBySource graph "Zeus"
-}
findEdgesBySource: Graph nData eData -> String -> List (Edge eData)
findEdgesBySource graph src =
   Dict.get src graph.relations |> Maybe.map .outbound |> Maybe.withDefault [] |> List.map (\r -> Dict.get r graph.edges) |> List.filterMap identity

{-| Find edge models by destination.

    findEdgesByDestination graph "Athena"
-}
findEdgesByDestination: Graph nData eData -> String -> List (Edge eData)
findEdgesByDestination graph dest =
   Dict.get dest graph.relations |> Maybe.map .inbound |> Maybe.withDefault [] |> List.map (\r -> Dict.get r graph.edges) |> List.filterMap identity

{-| Find the relations given the nodeId.

    findRelations graph "Athena"
-}
findRelations:  Graph nData eData -> String -> Maybe Relations
findRelations graph nodeId =
  Dict.get nodeId graph.relations

{-| Export the graph as a list of paths faciliting a xpath like approach.
-}
toPaths:  Graph nData eData -> GraphPaths
toPaths graph = graph.paths


-- FOR INTERNAL USE ONLY
-- Private methods

unique: List comparable -> List comparable
unique list =
  Set.fromList list |> Set.toList

createRelations: List (Edge eData) ->  Dict String Relations
createRelations edges =
  let
    simpleEdges = edges |> List.map (\e -> {e | value = Irrelevant})
    outboundEdges = simpleEdges |> groupBy .source
    inboundEdges = simpleEdges |> groupBy .destination
    keys = List.append (Dict.keys outboundEdges)  (Dict.keys inboundEdges) |> unique
    createRel: String -> Relations
    createRel k = createNodeRelations (Dict.get k inboundEdges) (Dict.get k outboundEdges)
    relations = keys |> List.map (\k -> (k,createRel k))
  in
    Dict.fromList relations

-- create node relations from edge
createNodeRelations: Maybe (List { source : String, value : Irrelevant, destination : String }) ->  Maybe (List { source : String, value : Irrelevant, destination : String }) -> Relations
createNodeRelations inbound outbound =
  let
      toSourceDest value= value |> Maybe.withDefault [] |> List.map (\v -> (v.source, v.destination))
      inbounds = inbound |> toSourceDest
      outbounds = outbound |> toSourceDest
  in
    Relations.create  inbounds outbounds   

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

swapTuple: (a, b) -> (b, a)
swapTuple value = (Tuple.second value, Tuple.first value)

-- get nodeIds that are been used in a relation (edge)
getActiveNodeIds: Dict String Relations ->  Dict String Int
getActiveNodeIds dict =
    Dict.keys dict |> List.indexedMap (,) |> List.map swapTuple |> Dict.fromList

type alias FlaggedPath = (Bool, (List String))

-- assumption: keys should never be empty
directAncestors: Dict String Relations -> List FlaggedPath -> List FlaggedPath
directAncestors relations lists =
  let
      ids = List.head lists |> Maybe.withDefault (False, []) |> second -- keys should never be empty
      lastId = List.reverse ids |> List.head |> Maybe.withDefault "???" -- should at least have one id
      parents = Dict.get lastId relations |> Maybe.map .inbound |> Maybe.withDefault []
  in
      case parents of
        [] ->
          [(True, ids)] --root
        [one] ->
          [(False, ids ++ [first one])] |> directAncestors relations -- single ancestor
        many ->
          List.map (\k -> (False, ids ++ [first k])) many --multiple ancestors


type alias PathBuilder = {
  paths: List (List String)
  , progress: List (List String) 
}

buildPaths: Dict String Relations -> PathBuilder -> PathBuilder
buildPaths relations builder =
    let
        ancestors = builder.progress |> List.map (\k -> directAncestors relations [(False, k)]) |> List.concat
        (roots, tocontinue) = List.partition first ancestors
        newbuilder = {
          paths = List.append builder.paths (List.map second roots)
          , progress = List.map second tocontinue
        }
    in
       if (List.isEmpty tocontinue) then
        newbuilder
       else
        buildPaths relations newbuilder

-- expand path [a, b, c] to [a], [a, b], [a, b, c]    
expandPath: List String -> List (List String)
expandPath path =
  case path of
    [] ->
      [path]
    [one] ->
      [path]
    twoOrMore ->
      List.tail twoOrMore |> Maybe.withDefault [] |> expandPath |> (\expanded-> path :: expanded)

-- get the edges a the leaf level
getLeaves: List Relations -> List (String, String)
getLeaves list =
  List.filter Relations.isLeaf list |> List.map .inbound |> List.concat

createGraphPaths: Dict String Relations -> GraphPaths
createGraphPaths relations =
  let
      pathBuilder = buildPaths relations {
        paths = []
        , progress = Dict.values relations |> getLeaves |> List.map (\leave -> [Tuple.second leave, Tuple.first leave])
      }
      paths = pathBuilder.paths |> List.map expandPath |> List.concat |> unique |> List.map (Path.create (getActiveNodeIds relations))
  in
      GraphPaths.create paths
