port module Main exposing (..)

import GraphTests
import GraphPathsTests
import ValueKeyTests
import ValueAccessTests
import Test.Runner.Node exposing (run, TestProgram)
import Test exposing (Test)
import Json.Encode exposing (Value)


all : Test
all =
    Test.concat
        [ GraphTests.all
        , GraphPathsTests.all
        , ValueKeyTests.all
        , ValueAccessTests.all
        ]

main : TestProgram
main =
    run emit all


port emit : ( String, Value ) -> Cmd msg
