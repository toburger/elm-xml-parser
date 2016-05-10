module Main (..) where

import Graphics.Element exposing (show)
import Test.Xml.Sample as Xml
import Xml.Parser exposing (parseXml)


main : Graphics.Element.Element
main =
  show <| parseXml Xml.sample1
