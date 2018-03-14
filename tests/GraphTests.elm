module GraphTests exposing (..)

import Test exposing (describe, test, Test)
import Expect
import Bubblegum.Graph exposing (..)
import FixtureHelper exposing (createSimpleGraph)

import List

myGraph = createSimpleGraph

all : Test
all =
    describe "Bubblegum.GraphFinder"
        [describe "find nodes" <|
            [ test "find a node by id" <|
                \() ->
                    Expect.equal (findNode myGraph "node_3" |> Maybe.map .id) (Just "node_3") 
            ]
        , describe "find edges" <|
            [ test "find edges by a source node" <|
                \() ->
                    Expect.equal (findEdgesBySource myGraph "node_3" |> List.map .value) (["edge_3_4", "edge_3_5", "edge_3_10"]) 
            , test "find edges by a destination node" <|
                \() ->
                    Expect.equal (findEdgesByDestination myGraph "node_5" |> List.map .value) (["edge_3_5"]) 
            ]       
        ]
