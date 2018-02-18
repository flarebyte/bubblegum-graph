module Tests exposing (..)

import Test exposing (describe, test, Test)
import Expect
import Bubblegum.Graph exposing (createNode, createEdge, createGraph, findNodeModel, findEdgeModelsBySource)
import List
import Array

asNode: Int -> String
asNode num =
    "node_" ++ toString(num)

nodes=List.range 0 10 |> List.map (\i -> createNode (asNode i) (asNode i))

createMyEdge src dest = createEdge (["edge", toString(src), toString(dest)] |> String.join "_") (asNode src) (asNode dest) (["edge_value", toString(src), toString(dest)] |> String.join "_")

edges = [createMyEdge 1 2, createMyEdge 2 3, createMyEdge 3 4, createMyEdge 3 5, createMyEdge 5 6, createMyEdge 6 7, createMyEdge 6 8 ]


myGraph = createGraph nodes edges

all : Test
all =
    describe "Bubblegum.Graph"
        [ describe "createGraph" <|
            [ test "create a graph" <|
                \() ->
                    Expect.equal (List.length myGraph.edges) 7  
            ]
            , describe "find nodes" <|
            [ test "find a node by id" <|
                \() ->
                    Expect.equal (findNodeModel myGraph (asNode 3) |> Maybe.map .id) (Just "node_3") 
            ]
             , describe "find edges" <|
            [ test "find edges by a source node" <|
                \() ->
                    Expect.equal (findEdgeModelsBySource myGraph (asNode 3) |> List.map .id) (["edge_3_4", "edge_3_5"]) 
            ]
        ]
