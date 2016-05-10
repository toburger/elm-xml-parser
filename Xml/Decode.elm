module Xml.Decode (toJson) where

import Json.Encode exposing (string, object, list, array)
import Xml.Parser exposing (XmlAst(..))
import List.Extra as List
import Array


getElementsOrArray : List XmlAst -> List ( String, Json.Encode.Value )
getElementsOrArray elems =
  let
    -- partition the list by determining if a given key exists more than one time -> array
    ( array', elems' ) =
      elems
        |> List.map toJson'
        |> List.groupBy (\a b -> fst a == fst b)
        |> List.partition (\ls -> List.length ls > 1)

    -- flatten the "normal" elements
    elems'' =
      elems'
        |> List.concatMap identity

    -- create the array elements
    array'' =
      array'
        |> List.map
            (\ls ->
              let
                name =
                  ls
                    |>
                      List.head
                    |>
                      Maybe.map fst
                    -- cannot happen, there is always at least one element
                    -- in the list by the nature of how groupBy works
                    |>
                      Maybe.withDefault ""
              in
                ( name
                , ls
                    |> List.map snd
                    |> Array.fromList
                    |> array
                )
            )
  in
    elems'' ++ array''


getAttributes : List ( String, String ) -> List ( String, Json.Encode.Value )
getAttributes attrs =
  attrs
    |> List.map (\( n, v ) -> ( "__" ++ n, string v ))


toJson' : XmlAst -> ( String, Json.Encode.Value )
toJson' xmlAst =
  case xmlAst of
    Body txt ->
      ( "__text", string txt )

    Comment comment ->
      ( "__comment", string comment )

    Element name [] [ Body txt ] ->
      ( name, string txt )

    Element name attrs elems ->
      ( name, object (getAttributes attrs ++ getElementsOrArray elems) )


toJson : XmlAst -> Json.Encode.Value
toJson xmlAst =
  object [ toJson' xmlAst ]
