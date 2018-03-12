module Bubblegum.ValueAccess exposing(..)

{-| This library provides a directed graph model for representing relationships between UI components.

# Build
@docs

-}

import Dict exposing(Dict)
import Tuple exposing(first, second)
import Bubblegum.ValueKey as ValueKey

{-| 
-}
type alias Values m = Dict String (String, m)

get: Values  m-> String -> Maybe (String , m)
get values id =
  Dict.get id values

set: Values  m-> (String, m) -> Values  m
set values keyValue =
   Dict.insert (first keyValue) keyValue values

fromList: List (String, m) ->  Values  m
fromList values =
 values |> List.map (\v -> ((first v), v)) |> Dict.fromList

union: Values  m -> Values  m -> Values  m
union a b =
  Dict.union a b

keys: Values  m -> List String
keys values =
  Dict.keys values

matchAnyKeyOf: List String -> (String, m) -> Bool
matchAnyKeyOf keys keyValue =
  List.member (first keyValue) keys

getMany: Values  m-> List String -> List (String , m)
getMany values keys =
  Dict.toList values |> List.map second |> List.filter (matchAnyKeyOf keys)

descendants: Values  m -> String -> List (String, m)
descendants values key=
   ValueKey.descendants (Dict.keys values) key |> getMany values


