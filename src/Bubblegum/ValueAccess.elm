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

isJust : Maybe a -> Bool
isJust m =
  case m of
    Nothing -> False
    Just _  -> True

has: Values  m-> String -> Bool
has values id =
  get values id |> isJust

set: Values  m-> (String, m) -> Values  m
set values keyValue =
   Dict.insert (first keyValue) keyValue values

fromList: List (String, m) ->  Values  m
fromList values =
 values |> List.map (\v -> ((first v), v)) |> Dict.fromList

toList: Values  m -> List (String, m)
toList values =
  Dict.toList values |> List.map second 

union: Values  m -> Values  m -> Values  m
union a b =
  Dict.union a b

keys: Values  m -> List String
keys values =
  Dict.keys values

matchAnyKeyOf: List String -> (String, m) -> Bool
matchAnyKeyOf keys keyValue =
  List.member (first keyValue) keys

remove: Values  m -> String -> Values  m
remove values key =
  Dict.remove key values

getMany: Values  m-> List String -> List (String , m)
getMany values keys =
  toList values |> List.filter (matchAnyKeyOf keys)

descendants: Values  m -> String -> List (String, m)
descendants values key=
   ValueKey.descendants (Dict.keys values) key |> getMany values


