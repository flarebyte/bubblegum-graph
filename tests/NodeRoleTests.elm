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
                    Expect.equal (myMajorNodes |> .root) (Set.fromList(["node_1"])) 
            ]
        , describe "find leaf nodes" <|
            [ test "find the leaf nodes" <|
                \() ->
                    Expect.equal (myMajorNodes |> .leaf) (Set.fromList(["node_4", "node_7", "node_8", "node_17","node_18"])) 
            ]
        , describe "find convergence nodes" <|
            [ test "find the convergence nodes" <|
                \() ->
                    Expect.equal (myMajorNodes |> .convergence) (Set.fromList(["node_15", "node_20"])) 
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
