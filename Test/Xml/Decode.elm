module Main exposing (..)

import Xml.Decode exposing (..)
import Xml.Parser exposing (XmlAst(..), parseXml)
import Test.Xml.Sample as Xml
import Json.Encode exposing (encode)
import Html exposing (pre, text)


xml : List XmlAst
xml =
  parseXml Xml.sample1
    |> Result.withDefault [ Comment "error" ]


main : Html.Html msg
main =
  pre [] [ text (encode 4 (toJson xml)) ]
