module ValueKeyTests exposing (..)

import Test exposing (describe, test, Test)
import Expect
import Bubblegum.ValueKey exposing (..)


all : Test
all =
    describe "Bubblegum.valueKey"
        [ describe "from string" <|
            [ test "for a valid key with 3 digits padding" <|
                \() ->
                    Expect.equal (fromStr "017:006/007/008/009") ({pathId = 17, indices = [6,7,8,9]})
            ]
            , describe "to string" <|
            [ test "for key in range" <|
                \() ->
                    Expect.equal (toStr {pathId = 17, indices = [6,7,8,9]}) ("017:006/007/008/009")
              ,
              test "for key with indices over 1000" <|
                \() ->
                    Expect.equal (toStr {pathId = 17, indices = [6,7777,8,9]}) ("017:006/7777/008/009")       
            ]
        ]
