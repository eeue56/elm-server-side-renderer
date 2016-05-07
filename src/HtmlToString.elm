module HtmlToString exposing (..) -- where

import Html exposing (Html)
import Json.Decode exposing ((:=))
import String
import Helpers exposing (..)

type NodeType
    = TextTag TextTagRecord
    | NodeEntry NodeRecord
    | NoOp

type alias TextTagRecord =
    { text : String }

type alias NodeRecord =
    { tag : String
    --, facts : List String
    --, children : NodeType
    --, namespace : String
    --, descendantsCount : Int
    }


decodeNodeType : (String -> Result String NodeType) -> Json.Decode.Decoder NodeType
decodeNodeType typeFromString =
    ( "type" := Json.Decode.customDecoder Json.Decode.string (typeFromString))

decodeTextTag : Json.Decode.Decoder TextTagRecord
decodeTextTag =
    ( "ext" := Json.Decode.customDecoder Json.Decode.string (\text -> Ok { text = text }))

decodeNode : Json.Decode.Decoder NodeRecord
decodeNode =
    Json.Decode.object1 NodeRecord
        ( "tag" := Json.Decode.string )


typeFromString : String -> String -> Result String NodeType
typeFromString stuff type' =
    case type' of
        "text" ->
            Result.map TextTag (Json.Decode.decodeString decodeTextTag stuff)
        "node" ->
            Result.map NodeEntry (Json.Decode.decodeString decodeNode stuff)
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
        NodeEntry {tag} ->
            String.join ""
                [ "<"
                , tag
                , ">"
                , "</"
                , tag
                , ">"
                ]
        NoOp ->
            ""
