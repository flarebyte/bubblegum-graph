module FixtureHelper exposing (createSimpleGraph)

import Bubblegum.GraphBuilder exposing (..)

import List
import Tuple

asNode: Int -> String
asNode num =
    "node_" ++ toString(num)

createSimpleEdge: (Int, Int) -> Edge String
createSimpleEdge edge = createEdge (asNode (Tuple.first edge)) (asNode (Tuple.second edge)) (["edge", toString((Tuple.first edge)), toString(Tuple.second edge)] |> String.join "_")

createEdges: List (Int, Int) -> List (Edge String)
createEdges edges =
    edges |> List.map createSimpleEdge

createSimpleGraph: Graph String String
createSimpleGraph = 
    let
        simpleNodes=List.range 0 25 |> List.map (\i -> createNode (asNode i) (asNode i))
        simpleEdges =  createEdges [
            (1,2), (2,3), (3,4), (3,5), (3, 10), (5, 6), (6, 7), (6, 8), (5, 20), (10, 20), (6, 15)
            , (15, 16), (16, 17), (16, 18), (16, 4)
            , (20, 7), (20, 15)
        ]       
    in
        createGraph simpleNodes simpleEdges
