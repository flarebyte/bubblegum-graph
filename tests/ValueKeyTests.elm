module ValueKeyTests exposing (..)

import Test exposing (describe, test, Test)
import Expect
import Bubblegum.ValueKey exposing (..)

myKeys = [
    "001/001/001:001/001/001"
    ,"001/001:001/001"
    ,"001:001"
    ,"001/002:001/001"
    ,"001/001/001/001:001/001/001/001"
    , "001/001/001/002:001/001/001/001"
    , "001/001/001/003:001/001/001/001"
    , "001/001/002/001:001/001/001/001"
    , "001/001/003/001:001/001/001/001"
    , "001/002/001/001:001/001/001/001"
    , "001/003/001/001:001/001/001/001"
    , "002/001/001/001:001/001/001/001"
    ]

all : Test
all =
    describe "Bubblegum.ValueKey"
        [ describe "from string" <|
            [ test "for a valid key with 3 digits padding" <|
                \() ->
                    Expect.equal (fromStr "004/006/007/009:006/007/008/009") ({path = [9,7,6, 4], indices = [9,8,7,6]})
            ]
            , describe "to string" <|
            [ test "for key in range" <|
                \() ->
                    Expect.equal (toStr {path = [9,7,6, 4], indices = [6,7,8,9]}) ("004/006/007/009:009/008/007/006")
              ,
              test "for key with indices over 1000" <|
                \() ->
                    Expect.equal (toStr {path = [9,7,6, 4], indices = [6,7777,8,9]}) ("004/006/007/009:009/008/7777/006")       
            ]
            , describe "get the child indice" <|
            [ test "the head indice" <|
                \() ->
                    Expect.equal (childIndice "001/001/001/001:006/007/008/009") 9
            ]
            , describe "increment a key" <|
            [ test "by +1" <|
                \() ->
                    Expect.equal (incrementKey 1 "001/001/001/001:006/007/008/009") "001/001/001/001:006/007/008/010"
               , test "by -1" <|
                \() ->
                    Expect.equal (incrementKey -1 "001/001/001/001:010/007/008/009") "001/001/001/001:010/007/008/008"     
            ]
            , describe "descendants of key" <|
            [ test "with grand children" <|
                \() ->
                    Expect.equal (List.filter (isDescendant "001/001:001/001") myKeys) [
                        "001/001/001:001/001/001"
                        ,"001/001/001/001:001/001/001/001"
                        ,"001/001/001/002:001/001/001/001"
                        ,"001/001/001/003:001/001/001/001"
                        ,"001/001/002/001:001/001/001/001"
                        ,"001/001/003/001:001/001/001/001"
                        ]
            ]
            , describe "descendants or self of key" <|
            [ test "with grand children or self" <|
                \() ->
                    Expect.equal (List.filter (isDescendantOrSelf "001/001:001/001") myKeys) [
                        "001/001/001:001/001/001"
                        ,"001/001:001/001"
                        ,"001/001/001/001:001/001/001/001"
                        ,"001/001/001/002:001/001/001/001"
                        ,"001/001/001/003:001/001/001/001"
                        ,"001/001/002/001:001/001/001/001"
                        ,"001/001/003/001:001/001/001/001"]
            ]
            , describe "ancestor of a key" <|
            [ test "when grand parents" <|
                \() ->
                    Expect.equal (List.filter (isAncestor "001/001/001/001:001/001/001/001") myKeys) [
                        "001/001/001:001/001/001"
                        ,"001/001:001/001"
                        ,"001:001"
                        ]
            ]
        ]
