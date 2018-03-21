module Bubblegum.Graph exposing(Graph, create, findNode, findEdge, findEdgesBySource, findEdgesByDestination)

{-| This library provides a directed graph model for representing relationships between UI components.

# Build
@docs createNode, createEdge, createGraph

-}

import Dict exposing(Dict)
import Set exposing(Set)
import Tuple exposing (first, second)
import String exposing (padLeft)
import Bubblegum.Irrelevant exposing(..)
import Bubblegum.Node as Node exposing(..)
import Bubblegum.Edge as Edge exposing(..)
import Bubblegum.Relations as Relations exposing(..)
import Bubblegum.GraphPaths as GraphPaths exposing(GraphPaths)

{-| The core representation of a value.
-}
type alias Graph nData eData = {
    nodes: Dict String (Node nData)
    , edges: Dict (String, String) (Edge eData)
    , relations: Dict String Relations
    , paths: GraphPaths
  }

{-| Create graph.
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

unique: List comparable -> List comparable
unique list =
  Set.fromList list |> Set.toList

{-| Create graph.
-}
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

{-| Create graph.
-}
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
swapTuple value =
  (Tuple.second value, Tuple.first value)

getActiveNodeIds: Dict String Relations ->  Dict String Int
getActiveNodeIds dict =
    Dict.keys dict |> List.indexedMap (,) |> List.map swapTuple |> Dict.fromList


{-| find node model.
-}
findNode: Graph nData eData -> String -> Maybe (Node nData)
findNode graph id =
  Dict.get id graph.nodes

{-| find edge models by destination.
-}
findEdge: Graph nData eData -> (String, String) -> Maybe (Edge eData)
findEdge graph sourceDest =
   Dict.get sourceDest graph.edges 

{-| find edge models by source.
-}
findEdgesBySource: Graph nData eData -> String -> List (Edge eData)
findEdgesBySource graph src =
   Dict.get src graph.relations |> Maybe.map .outbound |> Maybe.withDefault [] |> List.map (\r -> Dict.get r graph.edges) |> List.filterMap identity

{-| find edge models by destination.
-}
findEdgesByDestination: Graph nData eData -> String -> List (Edge eData)
findEdgesByDestination graph dest =
   Dict.get dest graph.relations |> Maybe.map .inbound |> Maybe.withDefault [] |> List.map (\r -> Dict.get r graph.edges) |> List.filterMap identity

padInt: Int -> String
padInt int =
  padLeft 3 '0' (toString int)

joinInt: List Int -> String
joinInt list = 
  list |> List.reverse |> List.map padInt |> String.join "/"


type alias FlaggedPath = (Bool, (List String))

-- keys should never be empty
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
          List.map (\k -> (False, ids ++ [first k])) many


type alias PathBuilder = {
  paths: List (List String)
  , progress: List (List String) 
}

buildPaths: Dict String Relations -> PathBuilder -> PathBuilder
buildPaths relations builder =
    let
        ancestors = builder.progress |> List.map (\k -> directAncestors relations [(False, k)]) |> List.concat
        (roots, tocontinue) = List.partition (\a -> first a ) ancestors
        newbuilder = {
          paths = List.append builder.paths (List.map second roots)
          , progress = List.map second tocontinue
        }
    in
       if (List.isEmpty tocontinue) then
        newbuilder
       else
        buildPaths relations newbuilder
    
expandPath: List String -> List (List String)
expandPath path =
  case path of
    [] ->
      [path]
    [one] ->
      [path]
    twoOrMore ->
      List.tail twoOrMore |> Maybe.withDefault [] |> expandPath |> (\stuff-> path :: stuff)


createGraphPaths: Dict String Relations -> GraphPaths
createGraphPaths relations =
  let
      nodeIds = getActiveNodeIds relations
      nodeIdToInt id = Dict.get id nodeIds |> Maybe.withDefault -1
      createId ids = ids |> List.map nodeIdToInt |> joinInt
      createPath ids = {id = createId ids, nodeIds=ids}
      leaves = Dict.values relations |> List.filter Relations.isLeaf |> List.map .inbound |> List.concat
      pathBuilder = buildPaths relations {
        paths = []
        , progress = leaves |> List.map (\leave -> [Tuple.second leave, Tuple.first leave])
      }
      paths = pathBuilder.paths |> List.map expandPath |> List.concat |> unique |> List.map createPath
  in
      GraphPaths.create paths
