module Bubblegum.ValueDict exposing(insert, update, remove, isEmpty, member, get, size, keys, toList, fromList, union)

{-| A specialized dictionary mapping unique keys to values.

# Build
@docs insert, update, remove

# Query
@docs isEmpty, member, get, size

# Lists
@docs keys, toList, fromList

# Transform
Most transformations could be done after exporting with toList.

# Combine
@docs union
-}


import Dict exposing(Dict)
import Tuple exposing(first, second)

type alias Values m = Dict String (String, m)

{-| Determine if the dictionary is empty.

    isEmpty empty == True
-}
isEmpty : Values  m -> Bool
isEmpty values =
  Dict.isEmpty values

{-| Determine the number of key-value pairs in the dictionary. -}
size : Values  m -> Int
size values =
  Dict.size values

{-| Get the value associated with a value-key. If the key is not found, return
`Nothing`.

    get values "004/006/007/011:006/007/008/009" == ("004/006/007/011:006/007/008/009", "some value")
-}
get: Values  m-> String -> Maybe (String , m)
get values id =
  Dict.get id values

{-| Check whether there is value-key in values.

    member values "004/006/007/011:006/007/008/009" == True
-}
member: Values  m-> String -> Bool
member values id =
  member values id

{-| Insert a value into values. Replaces value when there is
a collision. This will take *O(log n)* time.

    insert values ("004/006/007/011:006/007/008/009", "some value")

-}
insert: Values  m-> (String, m) -> Values  m
insert values keyValue =
   Dict.insert (first keyValue) keyValue values

{-| Convert an association list into values . -}
fromList: List (String, m) ->  Values  m
fromList values =
 values |> List.map (\v -> ((first v), v)) |> Dict.fromList

{-| Convert values into an association list of key-value pairs, sorted by keys. -}
toList: Values  m -> List (String, m)
toList values =
  Dict.values values

{-| Combine two values objects. If there is a collision, preference is given
to the first values object.
-}
union: Values  m -> Values  m -> Values  m
union a b =
  Dict.union a b

{-| Get all of the keys in values, sorted from lowest to highest.

    keys values
-}
keys: Values  m -> List String
keys values =
  Dict.keys values

{-| Remove a key-value pair from values. If the key is not found,
no changes are made. This will take *O(log n)* time.

    remove values ("004/006/007/011:006/007/008/009", "some value")

-}
remove: Values  m -> String -> Values  m
remove values key =
  Dict.remove key values

{-| Update the value of a values object for a specific key with a given function. -}
update:  Values  m -> String -> (Maybe (String, m) -> Maybe (String, m)) -> Values  m
update values key transf =
  Dict.update key transf values
