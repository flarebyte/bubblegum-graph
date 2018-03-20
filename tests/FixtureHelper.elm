module FixtureHelper exposing (createSimpleGraph, end2endPathsForSimpleGraph)

import Bubblegum.Node as Node exposing (..)
import Bubblegum.Edge as Edge exposing (..)
import Bubblegum.Graph as Graph exposing (..)

import List
import Tuple

asNode: Int -> String
asNode num =
    "node_" ++ toString(num)

createSimpleEdge: (Int, Int) -> Edge String
createSimpleEdge edge = Edge.create (asNode (Tuple.first edge)) (asNode (Tuple.second edge)) (["edge", toString((Tuple.first edge)), toString(Tuple.second edge)] |> String.join "_")

createEdges: List (Int, Int) -> List (Edge String)
createEdges edges =
    edges |> List.map createSimpleEdge

createSimpleGraph: Graph String String
createSimpleGraph = 
    let
        simpleNodes=List.range 0 25 |> List.map (\i -> Node.create (asNode i) (asNode i))
        simpleEdges =  createEdges [
            (1,2), (2,3), (3,4), (3,5), (3, 10), (5, 6), (6, 7), (6, 8), (5, 20), (10, 20), (6, 15)
            , (15, 16), (16, 17), (16, 18), (16, 4)
            , (20, 7), (20, 15)
        ]       
    in
        Graph.create simpleNodes simpleEdges

end2endPathsForSimpleGraph = [
    ["node_4","node_3","node_2","node_1"],
    ["node_7","node_6","node_5","node_3","node_2","node_1"],
    ["node_8","node_6","node_5","node_3","node_2","node_1"],
    ["node_17","node_16","node_15","node_6","node_5","node_3","node_2","node_1"],
    ["node_18","node_16","node_15","node_6","node_5","node_3","node_2","node_1"],
    ["node_4","node_16","node_15","node_6","node_5","node_3","node_2","node_1"],
    ["node_7","node_20","node_5","node_3","node_2","node_1"],
    ["node_7","node_20","node_10","node_3","node_2","node_1"],
    ["node_17","node_16","node_15","node_20","node_5","node_3","node_2","node_1"],
    ["node_17","node_16","node_15","node_20","node_10","node_3","node_2","node_1"],
    ["node_18","node_16","node_15","node_20","node_5","node_3","node_2","node_1"],
    ["node_18","node_16","node_15","node_20","node_10","node_3","node_2","node_1"],
    ["node_4","node_16","node_15","node_20","node_5","node_3","node_2","node_1"],
    ["node_4","node_16","node_15","node_20","node_10","node_3","node_2","node_1"]
    ]