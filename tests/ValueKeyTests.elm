module ValueKeyTests exposing (..)

import Test exposing (describe, test, Test)
import Expect
import Bubblegum.ValueKey exposing (..)


all : Test
all =
    describe "Bubblegum.ValueKey"
        [ describe "from string" <|
            [ test "for a valid key with 3 digits padding" <|
                \() ->
                    Expect.equal (fromStr "017:006/007/008/009") ({pathId = 17, indices = [9,8,7,6]})
            ]
            , describe "to string" <|
            [ test "for key in range" <|
                \() ->
                    Expect.equal (toStr {pathId = 17, indices = [6,7,8,9]}) ("017:009/008/007/006")
              ,
              test "for key with indices over 1000" <|
                \() ->
                    Expect.equal (toStr {pathId = 17, indices = [6,7777,8,9]}) ("017:009/008/7777/006")       
            ]
            , describe "get the child indice" <|
            [ test "the head indice" <|
                \() ->
                    Expect.equal (childIndice "017:006/007/008/009") 9
            ]
            , describe "increment a key" <|
            [ test "by +1" <|
                \() ->
                    Expect.equal (incrementKey 1 "017:006/007/008/009") "017:006/007/008/010"
               , test "by -1" <|
                \() ->
                    Expect.equal (incrementKey -1 "017:010/007/008/009") "017:010/007/008/008"     
            ]
        ]
