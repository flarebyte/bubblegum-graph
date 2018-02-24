module Tests exposing (..)

import Test exposing (describe, test, Test)
import Expect
import Bubblegum.GraphBuilder exposing (..)
import Bubblegum.GraphFinder exposing (..)
import Bubblegum.Graph exposing (..)

import List
import Set

asNode: Int -> String
asNode num =
    "node_" ++ toString(num)

nodes=List.range 0 25 |> List.map (\i -> createNode (asNode i) (asNode i))

createMyEdge src dest = createEdge (["edge", toString(src), toString(dest)] |> String.join "_") (asNode src) (asNode dest) (["edge_value", toString(src), toString(dest)] |> String.join "_")

reusedEdges = [createMyEdge 15 16, createMyEdge 16 17, createMyEdge 16 18, createMyEdge 16 4]
reusedContainer = [createMyEdge 20 7, createMyEdge 20 15]
edges = [createMyEdge 1 2, createMyEdge 2 3, createMyEdge 3 4, createMyEdge 3 5, createMyEdge 3 10,
 createMyEdge 5 6, createMyEdge 6 7, createMyEdge 6 8 , createMyEdge 5 20, createMyEdge 10 20, createMyEdge 6 15] 
 ++ reusedEdges ++ reusedContainer


myGraph = createGraph nodes edges
myMajorNodes = findMajorNodes myGraph

all : Test
all =
    describe "Bubblegum.Graph"
        [ describe "creation of graph" <|
            [ test "create a graph" <|
                \() ->
                    Expect.equal (List.length myGraph.edges) 17 
            ]
        , describe "find nodes" <|
            [ test "find a node by id" <|
                \() ->
                    Expect.equal (findNode myGraph (asNode 3) |> Maybe.map .id) (Just "node_3") 
            ]
        , describe "find edges" <|
            [ test "find edges by a source node" <|
                \() ->
                    Expect.equal (findEdgesBySource myGraph (asNode 3) |> List.map .id) (["edge_3_4", "edge_3_5", "edge_3_10"]) 
            , test "find edges by a destination node" <|
                \() ->
                    Expect.equal (findEdgesByDestination myGraph (asNode 5) |> List.map .id) (["edge_3_5"]) 
            ]
        , describe "find root nodes" <|
            [ test "find the root node" <|
                \() ->
                    Expect.equal (findRootNodeIds myGraph) (Set.fromList(["node_1"])) 
            ]
        , describe "find leaf nodes" <|
            [ test "find the leaf nodes" <|
                \() ->
                    Expect.equal (findLeafNodeIds myGraph) (Set.fromList(["node_4", "node_7", "node_8", "node_17","node_18"])) 
            ]
        , describe "find convergence nodes" <|
            [ test "find the convergence nodes" <|
                \() ->
                    Expect.equal (findConvergenceNodeIds myGraph) (Set.fromList(["node_15", "node_20"])) 
            ]
       , describe "find major nodes" <|
            [ test "find the root size" <|
                \() ->
                    Expect.equal (findMajorNodes myGraph |> .root |> Set.size) 1
              , test "find the leaf size" <|
                \() ->
                    Expect.equal (findMajorNodes myGraph |> .leaf |> Set.size) 5
               , test "find the convergence size" <|
                \() ->
                    Expect.equal (findMajorNodes myGraph |> .convergence |> Set.size) 2      
            ]
      , describe "find the role of node" <|
            [ test "identify a root node" <|
                \() ->
                    Expect.equal (findNodeRole myMajorNodes "node_1") (RootNode "node_1")
              , test "identify a leaf node" <|
                \() ->
                    Expect.equal (findNodeRole myMajorNodes "node_4") (LeafNode "node_4")
               , test "identify a convergence node" <|
                \() ->
                    Expect.equal (findNodeRole myMajorNodes "node_15") (ConvergenceNode "node_15")
               , test "identify any other node" <|
                \() ->
                    Expect.equal (findNodeRole myMajorNodes "node_100") (SimpleNode "node_100")      
            ]
       , describe "find the major parents" <|
            [ test "identify the immediate parent" <|
                \() ->
                    Expect.equal (findMajorParents myGraph myMajorNodes "node_16") ([ConvergenceNode "node_15"])
              , test "identify parent of 15" <|
                \() ->
                    Expect.equal (findMajorParents myGraph myMajorNodes "node_15") ([RootNode "node_1", ConvergenceNode "node_20"])
              , test "identify parent of 20" <|
                \() ->
                    Expect.equal (findMajorParents myGraph myMajorNodes "node_20") ([RootNode "node_1", RootNode "node_1"])
            ]
       , describe "find the major tree" <|
            [ test "gets the right nodes" <|
                \() ->
                    Expect.equal (findMajorGraph myGraph myMajorNodes |> .nodes) (
                       [nodeIdToNode("node_1"), nodeIdToNode("node_15"), nodeIdToNode("node_20")]
                  )
                , test "gets the right edges" <|
                 \() ->
                    Expect.equal (findMajorGraph myGraph myMajorNodes |> .edges) (
                        [
                            createEdge "" "node_1" "node_15" Irrelevant
                            , createEdge "" "node_20" "node_15" Irrelevant
                            , createEdge "" "node_1" "node_20" Irrelevant
                        ]
                  )
            ]
       
        ]
