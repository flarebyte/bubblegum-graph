module GraphIndexTests exposing (..)

import Test exposing (describe, test, Test)
import Expect
import Bubblegum.Node as Node
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
                    Expect.equal (findMajorParents myGraphIndex "node_16") ([Node.create "node_15" ConvergenceNode])
              , test "identify parent of 15" <|
                \() ->
                    Expect.equal (findMajorParents myGraphIndex "node_15") ([Node.create "node_20" ConvergenceNode, Node.create "node_1" RootNode])
              , test "identify parent of 20" <|
                \() ->
                    Expect.equal (findMajorParents myGraphIndex "node_20") ([Node.create "node_1" RootNode, Node.create "node_1" RootNode])
            ]
        ]
