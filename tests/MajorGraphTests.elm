module MajorGraphTests exposing (..)

import Test exposing (describe, test, Test)
import Expect
import Bubblegum.Irrelevant exposing(..)
import Bubblegum.Edge as Edge exposing(..)
import Bubblegum.Graph as Graph
import Bubblegum.MajorGraph exposing(..)
import Bubblegum.GraphIndex exposing (..)
import FixtureHelper exposing (createSimpleGraph)

myGraphIndex = toGraphIndex createSimpleGraph
myMajorGraph = toMajorGraph myGraphIndex

all : Test
all =
    describe "Bubblegum.MajorGraph"
        [ describe "find the major tree" <|
            [ test "gets the right nodes" <|
                \() ->
                    Expect.equal (myMajorGraph |> Graph.toNodeList |> List.map .id) (
                       ["node_1", "node_15", "node_20"]
                  )
                , test "gets the right edges" <|
                 \() ->
                    Expect.equal (myMajorGraph |> .edges) (
                        [
                            Edge.create  "node_1" "node_15" Irrelevant
                            , Edge.create "node_20" "node_15" Irrelevant
                            , Edge.create "node_1" "node_20" Irrelevant
                         ]
                  )
            ]
       
        ]
