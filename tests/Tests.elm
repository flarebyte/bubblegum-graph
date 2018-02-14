module Tests exposing (..)

import Test exposing (describe, test, Test)
import Expect
import Bubblegum.Graph exposing (createNode, createEdge, createGraph)
import List
import Array

asNode: Int -> String
asNode num =
    "node_" ++ toString(num)
    
nodes=List.range 0 10 |> List.map (\i -> createNode (asNode i) (asNode i))

edge1_2 = createEdge "edge1_2" (asNode 1) (asNode 2) "edge1_2"
edge2_3 = createEdge "edge2_3" (asNode 2) (asNode 3) "edge2_3"
edge3_4 = createEdge "edge3_4" (asNode 3) (asNode 4) "edge3_4"
edges = [edge1_2, edge2_3, edge3_4]

myGraph = createGraph nodes edges

all : Test
all =
    describe "Bubblegum.Graph"
        [ describe "createGraph" <|
            [ test "create a graph" <|
                \() ->
                    Expect.equal (List.length myGraph.edges) 3   
            ]
        ]
