module NodeRoleTests exposing (..)

import Test exposing (describe, test, Test)
import Expect
import Set
import Bubblegum.NodeRole exposing (..)
import FixtureHelper exposing (createSimpleGraph)

myGraph = createSimpleGraph
myMajorNodes = findMajorNodes myGraph

all : Test
all =
    describe "Bubblegum.NodeRole"
        [ describe "find root nodes" <|
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
                    Expect.equal (findNodeRole myMajorNodes "node_1") (RootNode)
              , test "identify a leaf node" <|
                \() ->
                    Expect.equal (findNodeRole myMajorNodes "node_4") (LeafNode)
               , test "identify a convergence node" <|
                \() ->
                    Expect.equal (findNodeRole myMajorNodes "node_15") (ConvergenceNode)
               , test "identify any other node" <|
                \() ->
                    Expect.equal (findNodeRole myMajorNodes "node_100") (SimpleNode)      
            ]
        ]
