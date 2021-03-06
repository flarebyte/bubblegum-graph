module ValueDictTests exposing (..)

import Test exposing (describe, test, Test)
import Expect
import Bubblegum.ValueDict exposing (..)

val: String -> (String, String)
val value =
    (value, value)

myValues = fromList [
    val "001:001/001/001/001"
    , val "001:002/001/001/001"
    , val "001:003/001/001/001"
    , val "001:001/002/001/001"
    , val "001:001/003/001/001"
    , val "001:001/003/002/001"
    , val "001:001/003/002/002"
    ]


all : Test
all =
    describe "Bubblegum.ValueAccess"
        [ describe "get a value" <|
            [ test "for existing key" <|
                \() ->
                    Expect.equal (get myValues "001:003/001/001/001") (Just (val "001:003/001/001/001"))
            ]
        ]
