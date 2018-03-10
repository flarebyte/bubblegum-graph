module Bubblegum.ValueKey exposing(fromStr,toStr, firstIndice, incrementKey)

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
  pathId: Int
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

fromStr: String -> ValueKey
fromStr key =
  let
      a = splitAsTuple ":" key
  in
  {
  pathId = Tuple.first a |> parsePaddedInt
  , indices = Tuple.second a |> String.split "/" |> List.map parsePaddedInt
  }

toStr: ValueKey -> String
toStr key =
    padInt(key.pathId) ++ ":" ++ (key.indices |> List.map padInt |> String.join "/")

firstIndice: String -> Int
firstIndice key =
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