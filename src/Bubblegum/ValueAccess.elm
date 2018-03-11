module Bubblegum.ValueAccess exposing(..)

{-| This library provides a directed graph model for representing relationships between UI components.

# Build
@docs

-}

import Dict exposing(Dict)
import Tuple exposing(first)

{-| 
-}
type alias Values m = {
  keyToValue: Dict String (String, m)
}

get: Values  m-> String -> Maybe (String , m)
get values id =
  Dict.get id values.keyToValue

set: Values  m-> (String, m) -> Values  m
set values keyValue =
   { keyToValue = Dict.insert (first keyValue) keyValue values.keyToValue }

fromList: List (String, m) ->  Values  m
fromList values =
 { keyToValue =  values |> List.map (\v -> ((first v), v)) |> Dict.fromList }

-- union: Values  m -> Values  m -> Values  m
-- union a b =
--   Dict.union 

getKeysByQuery: Values  m-> String -> List String
getKeysByQuery values query=
  []

