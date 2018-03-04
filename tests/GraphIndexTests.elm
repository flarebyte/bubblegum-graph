module GraphIndexTests exposing (..)

import Test exposing (describe, test, Test)
import Expect
import Bubblegum.GraphBuilder exposing (createNode)
import Bubblegum.NodeRole exposing(..)
import Bubblegum.GraphIndex exposing (..)
import FixtureHelper exposing (createSimpleGraph)

myGraph = createSimpleGraph
myGraphIndex = toGraphIndex myGraph

all : Test
all =
    describe "Bubblegum.GraphIndex"
        [  describe "find the major parents" <|
            [ test "identify the immediate parent" <|
                \() ->
                    Expect.equal (findMajorParents myGraphIndex "node_16") ([createNode "node_15" ConvergenceNode])
              , test "identify parent of 15" <|
                \() ->
                    Expect.equal (findMajorParents myGraphIndex "node_15") ([createNode "node_1" RootNode, createNode "node_20" ConvergenceNode])
              , test "identify parent of 20" <|
                \() ->
                    Expect.equal (findMajorParents myGraphIndex "node_20") ([createNode "node_1" RootNode, createNode "node_1" RootNode])
            ]
        ]
