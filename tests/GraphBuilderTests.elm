module GraphBuilderTests exposing (..)

import Test exposing (describe, test, Test)
import Expect
import Bubblegum.GraphBuilder exposing (..)
import FixtureHelper exposing (createSimpleGraph)

import List

myGraph = createSimpleGraph

all : Test
all =
    describe "Bubblegum.GraphBuilder"
        [ describe "creation of graph" <|
            [ test "create a graph" <|
                \() ->
                    Expect.equal (List.length myGraph.edges) 17 
            ]
        ]
