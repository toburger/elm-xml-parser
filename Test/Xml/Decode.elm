module Main (..) where

import Xml.Decode exposing (..)
import Xml.Parser exposing (XmlAst(..), parseXml)
import Test.Xml.Sample as Xml
import Json.Encode exposing (encode)
import Html exposing (pre, text)


xml : XmlAst
xml =
  parseXml Xml.sample1
    |> Result.withDefault (Comment "error")



-- |> Debug.log "parsed"


main : Html.Html
main =
  pre [] [ text (encode 4 (toJson xml)) ]
