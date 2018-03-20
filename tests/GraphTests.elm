module GraphTests exposing (..)

import Test exposing (describe, test, Test)
import Expect
import Bubblegum.Graph exposing (..)
import FixtureHelper exposing (..)
import Set

import List

myGraph = createSimpleGraph

graphIds = ["000/006/008/009","000/006/008/010/011/012","000/006/008/010/011/013","000/006/008/010/011/002/003/004","000/006/008/010/011/002/003/005","000/006/008/010/011/002/003/009","000/006/008/010/007/012","000/006/008/001/007/012","000/006/008/010/007/002/003/004","000/006/008/001/007/002/003/004","000/006/008/010/007/002/003/005","000/006/008/001/007/002/003/005","000/006/008/010/007/002/003/009","000/006/008/001/007/002/003/009"]

all : Test
all =
    describe "Bubblegum.Graph"
        [describe "find nodes" <|
            [ test "find a node by id" <|
                \() ->
                    Expect.equal (findNode myGraph "node_3" |> Maybe.map .id) (Just "node_3") 
            ]
        , describe "find edges" <|
            [ test "find edges by a source node" <|
                \() ->
                    Expect.equal (findEdgesBySource myGraph "node_3" |> List.map .value) (["edge_3_4","edge_3_5","edge_3_10"]) 
            , test "find edges by a destination node" <|
                \() ->
                    Expect.equal (findEdgesByDestination myGraph "node_5" |> List.map .value) (["edge_3_5"]) 
            ]  
        , describe "Finds all the paths" <|
            [ test "id of paths" <|
                \() ->
                    Expect.equal (myGraph.paths |> .paths |> List.map .id |> Set.fromList) (graphIds |> Set.fromList) 
            , test "id of paths" <|
                \() ->
                    Expect.equal (myGraph.paths |> .paths |> List.map .nodeIds |> Set.fromList) (end2endPathsForSimpleGraph |> Set.fromList) 
           ]
                 
        ]
