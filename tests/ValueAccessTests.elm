module ValueAccessTests exposing (..)

import Test exposing (describe, test, Test)
import Expect
import Bubblegum.ValueAccess exposing (..)

val: String -> (String, String)
val value =
    (value, value)

myValues = createValues [
    val "001:001/001/001/001"
    , val "001:002/001/001/001"
    , val "001:003/001/001/001"
    ]


all : Test
all =
    describe "Bubblegum.ValueAccess"
        [ describe "get a value" <|
            [ test "for a valid key with 3 digits padding" <|
                \() ->
                    Expect.equal (get myValues "001:001/001/001/001") ( Just "001:001/001/001/001")
            ]
        ]
