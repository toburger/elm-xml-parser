module Xml.Decode exposing (toJson)

import Json.Encode exposing (string, object, list, array)
import Xml.Parser exposing (XmlAst(..))
--import List.Extra as List
import Array


takeWhile : (a -> Bool) -> List a -> List a
takeWhile predicate list =
  case list of
    []      -> []
    x::xs   -> if (predicate x) then x :: takeWhile predicate xs
               else []


dropWhile : (a -> Bool) -> List a -> List a
dropWhile predicate list =
  case list of
    []      -> []
    x::xs   -> if (predicate x) then dropWhile predicate xs
               else list


span : (a -> Bool) -> List a -> (List a, List a)
span p xs = (takeWhile p xs, dropWhile p xs)


groupBy : (a -> a -> Bool) -> List a -> List (List a)
groupBy eq xs' =
  case xs' of
    [] -> []
    (x::xs) -> let (ys,zs) = span (eq x) xs
               in (x::ys)::groupBy eq zs


getElementsOrArray : List XmlAst -> List ( String, Json.Encode.Value )
getElementsOrArray elems =
  let
    -- partition the list by determining if a given key exists more than one time -> array
    ( array', elems' ) =
      elems
        |> List.map toJson'
        |> groupBy (\a b -> fst a == fst b)
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

    CDATA cdata ->
      ( "__cdata", string cdata )

    Element name [] [ Body txt ] ->
      ( name, string txt )

    Element name attrs elems ->
      ( name, object (getAttributes attrs ++ getElementsOrArray elems) )

toJson : List XmlAst -> Json.Encode.Value
toJson xmlAst =
  xmlAst
    |> List.map toJson'
    |> object
