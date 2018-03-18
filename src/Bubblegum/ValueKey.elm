module Bubblegum.ValueKey exposing(fromStr,toStr, childIndice, incrementKey, isDescendantOrSelf, isDescendant, isAncestor)

{-| This library provides a directed graph model for representing relationships between UI components.

# Build
@docs

-}

import String exposing (padLeft)
import List
import Result
import Maybe
import Tuple

{-| The core representation of a value.
-}
type alias ValueKey = {
  path: List Int
  , indices: List Int
}

padInt: Int -> String
padInt int =
  padLeft 3 '0' (toString int)

parsePaddedInt: String -> Int
parsePaddedInt str =
  String.toInt str |> Result.toMaybe |> Maybe.withDefault 0

splitAsTuple: String -> String -> (String, String)
splitAsTuple sep str =
     (
       String.split sep str |> List.head |> Maybe.withDefault ""
     , String.split sep str |> List.reverse |> List.head |> Maybe.withDefault ""
     )

splitInt: String -> List Int
splitInt value =
  String.split "/" value |> List.map parsePaddedInt |> List.reverse

fromStr: String -> ValueKey
fromStr key =
  let
      a = splitAsTuple ":" key
  in
  {
  path = Tuple.first a |> splitInt
  , indices = Tuple.second a |> splitInt
  }

joinInt: List Int -> String
joinInt list = 
  list |> List.reverse |> List.map padInt |> String.join "/"

toStr: ValueKey -> String
toStr key =
    (joinInt key.path) ++ ":" ++ (joinInt key.indices)

childIndice: String -> Int
childIndice key =
  fromStr key |> .indices |> List.head |> Maybe.withDefault 0

incFirstIndice: Int -> List Int -> List Int
incFirstIndice delta indices =
  let
    first = indices |> List.head |> Maybe.withDefault 0 |> (+) delta
    rest = indices |> List.tail |> Maybe.withDefault []
  in
    first :: rest

incValueKey: Int -> ValueKey -> ValueKey
incValueKey delta key =
  {key | indices = (incFirstIndice delta key.indices)}

incrementKey: Int -> String -> String
incrementKey delta key =
  fromStr key |> (incValueKey delta)|> toStr

asPath: String -> String
asPath key =
  splitAsTuple ":" key |> Tuple.first 

isDescendantOrSelf: String -> String -> Bool
isDescendantOrSelf self tested =
  String.startsWith (asPath self) (asPath tested)

isDescendant: String -> String -> Bool
isDescendant self tested =
  isDescendantOrSelf self tested && self /= tested

isAncestor:  String -> String -> Bool
isAncestor self tested =
  String.startsWith (asPath tested) (asPath self) && self /= tested