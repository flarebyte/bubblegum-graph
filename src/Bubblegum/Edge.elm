module Bubblegum.Edge exposing(create, Edge, toTuple, fromTuple)

{-| Edge helps to represent the relationship between two nodes.

# Build
@docs create, Edge

# Conversion
@docs toTuple, fromTuple

-}
import Tuple exposing (first, second)

{-| The representation of an edge between two nodes.
  
    {
      source = "London"
      , destination= "Paris"
      , value = "344 km"
    }

-}
type alias Edge eData= {
    source: String
    , destination: String
    , value: eData
 }

{-| Create an edge providing the source, the destination and the value.

    ldnParis = create "London" "Paris" "344 km"

-}
create: String -> String -> eData -> Edge eData
create source destination edgeData=
  {
    source = source
    , destination = destination
    , value = edgeData
 }

{-| Convert an edge to a tuple.

  toTuple ldnParis == (("London", "Paris"), "344 km")

-}
toTuple: Edge eData -> ((String, String), Edge eData)
toTuple edge =
  ((edge.source, edge.destination), edge)

{-| Convert a tuple to an edge.

  fromTuple (("London", "Paris"), "344 km") == { source = "London", destination = "Paris", value = "344 km"}

-}
fromTuple: ((String, String), Edge eData) -> Edge eData
fromTuple tuple =
  second(tuple)

