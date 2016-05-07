module HtmlToString exposing (..) -- where

import Html exposing (Html)
import Json.Decode exposing ((:=))
import String
import Helpers exposing (..)

type NodeType
    = TextTag TextTagRecord
    | NoOp

type alias TextTagRecord =
    { text : String }


decodeNodeType : (String -> Result String NodeType) -> Json.Decode.Decoder NodeType
decodeNodeType typeFromString =
    ( "type" := Json.Decode.customDecoder Json.Decode.string (typeFromString))

decodeTextTag : Json.Decode.Decoder TextTagRecord
decodeTextTag =
    ( "ext" := Json.Decode.customDecoder Json.Decode.string (\text -> Ok { text = text }))


typeFromString : String -> String -> Result String NodeType
typeFromString stuff type' =
    case type' of
        "text" ->
            Result.map TextTag (Json.Decode.decodeString decodeTextTag stuff)
        _ ->
            Err <| "No such type as " ++ type'


nodeTypeFromHtml : Html msg -> NodeType
nodeTypeFromHtml =
    toString
        >> String.split "="
        >> String.join ":"
        >> wrapFieldsWithQuotes
        >> (\x -> Json.Decode.decodeString (decodeNodeType (typeFromString x)) x)
        >> Result.withDefault NoOp

htmlToString : Html msg -> String
htmlToString node =
    case nodeTypeFromHtml node of
        TextTag {text} ->
            text
        NoOp ->
            ""
