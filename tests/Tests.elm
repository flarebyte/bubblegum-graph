module Tests exposing (..)

import Test exposing (describe, test, Test)
import Expect
import Bubblegum.Graph exposing (render)


all : Test
all =
    describe "Bubblegum.Graph"
        [ describe "createGraph" <|
            [ test "create a graph" <|
                \() ->
                    tripleEqual
                    (WidgetModel.fromTriples subjectId myIncSpinner |> WidgetModel.toTriples)
                    (myIncSpinner)               
              
           ,  test "create a medium-text" <|
                \() ->
                    tripleEqual
                    (WidgetModel.fromTriples subjectId myMediumText |> WidgetModel.toTriples)
                    (myMediumText)               
              
            ]
        ]
