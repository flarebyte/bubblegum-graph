module Bubblegum.ValueKey exposing(fromStr, toStr, childIndice, incrementKey, isDescendantOrSelf, isDescendant, isAncestor)

{-| ValueKey helps to represent the key of value in a ValueAccess object.

# Format

A value key has a format like "004/006/007/009:006/007/008/009". 
The values on left of the colon represents the path (list of nodeIds), sorted from ancestors to children.
The values on right of the colon represents the indices.
Internally, the ValueKey object store these values in reverse order, to make it easier to find the parent by applying List.tail.


# Assumptions
 
Indices and nodeIds should not exceed 1000 (3 digits). In practice, this should not be an issue.
This means the graph should not exceed 1000 nodes, otherwise some the functions may return unexpected values.
The benefit of this approach is that the length of the key is therefore determined by the number of nodes in the path.

# Conversion
@docs fromStr, toStr

# Operations on indices
@docs childIndice, incrementKey

# Predicates
@docs isDescendantOrSelf, isDescendant, isAncestor

-}

import String exposing (padLeft)
import List
import Result
import Maybe
import Tuple

{-| Convert a string to value key.
  
  fromStr "004/006/007/009:006/007/008/009" == {path = [9,7,6,4], indices = [9,8,7,6]}

-}
fromStr: String -> ValueKey
fromStr key =
  let
      a = splitAsTuple key
  in
  {
  path = Tuple.first a |> splitInt
  , indices = Tuple.second a |> splitInt
  }

{-| Convert a string to value key.
  
  toStr {path = [9,7,6,4], indices = [9,8,7,6]} ==  "004/006/007/009:006/007/008/009"

-}
toStr: ValueKey -> String
toStr key =
    (joinInt key.path) ++ ":" ++ (joinInt key.indices)

{-| Return the indice of the child node.
  
   childIndice  "004/006/007/011:006/007/008/009" == 9

-}
childIndice: String -> Int
childIndice key =
  fromStr key |> .indices |> List.head |> Maybe.withDefault 0

{-| Increment or decrement a key.
  
   incrementKey 1 "004/006/007/011:006/007/008/009" == "004/006/007/011:006/007/008/010"

-}
incrementKey: Int -> String -> String
incrementKey delta key =
  fromStr key |> (incValueKey delta)|> toStr

{-| Check if a key is a descendant or self of another key.
  
   isDescendantOrSelf "004/006/007/011:006/007/008/009" "004/006/007/011:006/007/008/009"

-}
isDescendantOrSelf: String -> String -> Bool
isDescendantOrSelf self tested =
  String.startsWith (asPath self) (asPath tested)

-- TODO is this the right logic ??

{-| Check if a key is a descendant of another key.
  
   isDescendant "004/006/007/011:006/007/008/009" "004/006/007/011:006/007/008/009"

-}
isDescendant: String -> String -> Bool
isDescendant self tested =
  isDescendantOrSelf self tested && self /= tested

{-| Check if a key is a ancestor of another key.
  
   isAncestor "004/006/007/011:006/007/008/009" "004/006/007/011:006/007/008/009"

-}
isAncestor:  String -> String -> Bool
isAncestor self tested =
  String.startsWith (asPath tested) (asPath self) && self /= tested

  -- FOR INTERNAL USE ONLY
  -- Private methods

-- The representation of value key. Path represents the path as a list of nodeIds. Indices represents the number of values for each nodeId.
type alias ValueKey = {
  path: List Int
  , indices: List Int
}

-- Pads numbers with leading zero.
-- padInt 7 == 007
padInt: Int -> String
padInt int =
  padLeft 3 '0' (toString int)

-- Parses a number with leading zero.
-- parsePaddedInt 007 == 7
parsePaddedInt: String -> Int
parsePaddedInt str =
  String.toInt str |> Result.toMaybe |> Maybe.withDefault 0

-- Splits the colon
-- splitAsTuple "12:10" == ("12", "10")
splitAsTuple: String -> (String, String)
splitAsTuple str =
     (
       String.split ":" str |> List.head |> Maybe.withDefault ""
     , String.split ":" str |> List.reverse |> List.head |> Maybe.withDefault ""
     )

-- Splits the slash and reverse
-- splitInt "12/10" == (10, 12)
splitInt: String -> List Int
splitInt value =
  String.split "/" value |> List.map parsePaddedInt |> List.reverse

-- join a list of integer and reverse
-- joinInt [1, 2] == "2/1"
joinInt: List Int -> String
joinInt list = 
  list |> List.reverse |> List.map padInt |> String.join "/"

-- increment the indice of the child
incFirstIndice: Int -> List Int -> List Int
incFirstIndice delta indices =
  let
    first = indices |> List.head |> Maybe.withDefault 0 |> (+) delta
    rest = indices |> List.tail |> Maybe.withDefault []
  in
    first :: rest

-- increment the indice for a value key
incValueKey: Int -> ValueKey -> ValueKey
incValueKey delta key =
  {key | indices = (incFirstIndice delta key.indices)}

-- extract the path part of the key
asPath: String -> String
asPath key =
  splitAsTuple key |> Tuple.first 
