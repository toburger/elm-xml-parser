module Xml.Parser exposing (parseXml, XmlAst(..))

import Combine exposing (..)
import Combine.Char exposing (..)
import Combine.Infix exposing (..)
import String
import Result as Result'


type alias Name =
  String


type alias Key =
  String


type alias Value =
  String


type alias Attribute =
  ( Key, Value )


type XmlAst
  = Element Name (List Attribute) (List XmlAst)
  | Body String
  | Comment String
  | CDATA String


spaces : Parser (List Char)
spaces =
  many (space <|> newline <|> tab)


letter : Parser Char
letter =
  upper <|> lower


betweenBoth : Char -> Parser String
betweenBoth ch =
  String.fromList
    <$> between
          (char ch)
          (char ch)
          (many1 ((noneOf [ ch ])) `or` succeed [])


betweenSingleQuotes : Parser String
betweenSingleQuotes =
  betweenBoth '\''


betweenDoubleQuotes : Parser String
betweenDoubleQuotes =
  betweenBoth '"'


quotedString : Parser String
quotedString =
  betweenSingleQuotes <|> betweenDoubleQuotes


attributeName : Parser String
attributeName =
  String.fromList
    <$> many1 (letter <|> digit <|> char '-' <|> char ':')
    <?> "Invalid Attribute name"


tagName : Parser String
tagName =
  String.fromList
    <$> many (choice [ letter, digit, char '_', char ':' ])
    <?> "Invalid Tag name"


keyValue : Parser ( String, String )
keyValue =
  (\key value -> ( key, value ))
    <$> (attributeName <* spaces <* char '=' <* spaces)
    <*> (quotedString <* spaces)


openTag : Parser ( String, List ( String, String ) )
openTag =
  (\name attribs -> ( name, attribs ))
    <$> (char '<' *> tagName)
    <*> (spaces *> many keyValue <* char '>')


closeTag : String -> Parser ()
closeTag str =
  ()
    <$ (string "</" *> spaces *> string str *> spaces *> char '>')
    <?> ("Expected closing Tag for " ++ toString str)


withExplicitCloseTag : Parser XmlAst
withExplicitCloseTag =
  (\( name, attribs, xml ) -> Element name attribs xml)
    <$> ((openTag <* spaces) `andThen` \( name, attribs ) -> (\xml -> ( name, attribs, xml )) <$> (many (innerXml <* spaces) <* closeTag name))


comment : Parser XmlAst
comment =
  (String.fromList >> String.trim >> Comment)
    <$> (string "<!--" *> manyTill anyChar (string "-->"))


cdata : Parser XmlAst
cdata =
  (String.fromList >> String.trim >> CDATA)
    <$> (string "<![CDATA[" *> manyTill anyChar (string "]]>"))


withoutExplicitCloseTag : Parser XmlAst
withoutExplicitCloseTag =
  (\name attribs -> Element name attribs [])
    <$> ((char '<' *> tagName <* spaces))
    <*> (many keyValue <* string "/>")


parseBody : Parser XmlAst
parseBody =
  (Body << String.trim << String.fromList) <$> (many1 (noneOf [ '<', '>' ]))


xmlDeclaration : Parser ()
xmlDeclaration =
  () <$ (string "<?xml" *> Combine.while ((/=) '?') <* string "?>")


xmlParser : Parser XmlAst
xmlParser =
  rec (\() -> withExplicitCloseTag) <|> rec (\() -> withoutExplicitCloseTag)


innerXml : Parser XmlAst
innerXml =
  comment <|> cdata <|> xmlParser <|> parseBody


parser : Parser XmlAst
parser =
  spaces *> maybe xmlDeclaration *> spaces *> xmlParser <* spaces <* end


parseXml : String -> Result'.Result (List String) XmlAst
parseXml str =
  case parse parser str of
    ( Ok xml, ctx ) ->
      Result'.Ok xml

    ( Err failure, _ ) ->
      Result'.Err failure
