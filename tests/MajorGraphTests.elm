module MajorGraphTests exposing (..)

import Test exposing (describe, test, Test)
import Expect
import Bubblegum.GraphBuilder exposing (..)
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
                    Expect.equal (myMajorGraph |> .nodes |> List.map .id) (
                       ["node_1", "node_15", "node_20"]
                  )
                , test "gets the right edges" <|
                 \() ->
                    Expect.equal (myMajorGraph |> .edges) (
                        [
                            createEdge "node_1" "node_15" Irrelevant
                            , createEdge "node_20" "node_15" Irrelevant
                            , createEdge "node_1" "node_20" Irrelevant
                         ]
                  )
            ]
       
        ]
