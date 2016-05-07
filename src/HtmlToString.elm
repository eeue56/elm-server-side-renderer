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
    , children : List NodeType
    --, facts : List String
    --, namespace : String
    --, descendantsCount : Int
    }

decodeNodeType : Json.Decode.Decoder NodeType
decodeNodeType =
    ( "type" := Json.Decode.string )
        |> (flip Json.Decode.andThen)
            (\typeString ->
                case typeString of
                    "text" ->
                        Json.Decode.map TextTag (decodeTextTag)
                    "node" ->
                        Json.Decode.map NodeEntry (decodeNode)
                    _ ->
                        Json.Decode.fail ("No such type as " ++ typeString)
            )

decodeTextTag : Json.Decode.Decoder TextTagRecord
decodeTextTag =
    ( "text" := Json.Decode.customDecoder Json.Decode.string (\text -> Ok { text = text }))

decodeNode : Json.Decode.Decoder NodeRecord
decodeNode =
    Json.Decode.object2 NodeRecord
        ( "tag" := Json.Decode.string )
        ( "children" := Json.Decode.list decodeNodeType)


nodeTypeFromHtml : Html msg -> NodeType
nodeTypeFromHtml =
    stringify
        >> Json.Decode.decodeString decodeNodeType
        >> Debug.log "decoderd:"
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
