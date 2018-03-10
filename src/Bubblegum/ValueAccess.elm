module Bubblegum.ValueAccess exposing(..)

{-| This library provides a directed graph model for representing relationships between UI components.

# Build
@docs

-}

import Dict exposing(Dict)

{-| 
-}
type alias Values m = {
  values: List (String, m)
  , keyToValue: Dict String m
}

createValues: List (String, m) -> Values m
createValues values =
  {
    values = values
    , keyToValue = values |>  Dict.fromList
  }


get: Values  m-> String -> Maybe m
get values id =
  Dict.get id values.keyToValue


