module Main exposing (..)

import Html exposing (code, text)
import Test.Xml.Sample as Xml
import Xml.Parser exposing (parseXml)


main : Html.Html msg
main =
  code [] [ text <| toString <| parseXml Xml.sample1 ]
