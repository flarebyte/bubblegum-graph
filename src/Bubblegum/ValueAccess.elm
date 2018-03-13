module Bubblegum.ValueAccess exposing(get, has, insert, fromList, toList, union, keys, remove, update)

{-| This library provides a directed graph model for representing relationships between UI components.

# Build
@docs

-}

import Dict exposing(Dict)
import Tuple exposing(first, second)

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

insert: Values  m-> (String, m) -> Values  m
insert values keyValue =
   Dict.insert (first keyValue) keyValue values

fromList: List (String, m) ->  Values  m
fromList values =
 values |> List.map (\v -> ((first v), v)) |> Dict.fromList

toList: Values  m -> List (String, m)
toList values =
  Dict.values values

union: Values  m -> Values  m -> Values  m
union a b =
  Dict.union a b

keys: Values  m -> List String
keys values =
  Dict.keys values

remove: Values  m -> String -> Values  m
remove values key =
  Dict.remove key values

update:  Values  m -> String -> (Maybe (String, m) -> Maybe (String, m)) -> Values  m
update values key transf =
  Dict.update key transf values
