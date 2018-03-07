port module Main exposing (..)

import GraphBuilderTests
import GraphFinderTests
import NodeRoleTests
import GraphIndexTests
import MajorGraphTests
import GraphPathsTests
import Test.Runner.Node exposing (run, TestProgram)
import Test exposing (Test)
import Json.Encode exposing (Value)


all : Test
all =
    Test.concat
        [ GraphBuilderTests.all
        , GraphFinderTests.all
        , NodeRoleTests.all
        , GraphIndexTests.all
        , MajorGraphTests.all
        , GraphPathsTests.all
        ]

main : TestProgram
main =
    run emit all


port emit : ( String, Value ) -> Cmd msg
