module GraphPathsTests exposing (..)

import Test exposing (describe, test, Test)
import Expect
import Set
import Bubblegum.GraphPaths exposing (..)

createMyPath: Int -> String -> Path
createMyPath id paths =
    {
    id = id
    , nodeIds = String.split "/" paths 
    }

path1 = createMyPath 1 "sister/dad/grand/ancestor"
path2 = createMyPath 2 "brother/dad/grand/ancestor"
path3 = createMyPath 3 "dad/grand/ancestor"
path4 = createMyPath 4 "grand/ancestor"
path5 = createMyPath 5 "ancestor"
path6 = createMyPath 6 "sister/mum/mumgrand/mumancestor"
path7 = createMyPath 7 "brother/mum/mumgrand/mumancestor"
path8 = createMyPath 8 "mum/mumgrand/mumancestor"
path9 = createMyPath 9 "mumgrand/mumancestor"
path10 = createMyPath 10 "mumancestor"
path11 = createMyPath 11 "uncle/grand/ancestor"
path12 = createMyPath 12 "/cousinuncle/grand/ancestor"

myPaths = createGraphPaths [path1, path2, path3, path4, path5, path6, path7, path8, path9, path10, path11, path12]

all : Test
all =
    describe "Bubblegum.GraphPaths"
        [ describe "find PathBy Id" <|
            [ test "for existing id" <|
                \() ->
                    Expect.equal (findPathById  myPaths 3) (Just path3)
            ]
         ]