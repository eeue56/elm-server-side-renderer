module ServerSide.InternalTypes
    exposing
        ( NodeType(..)
        , TextTagRecord
        , NodeRecord
        , CustomNodeRecord
        , MarkdownNodeRecord
        , Facts
        , decodeNodeType
        )

-- where

import Dict exposing (Dict)
import Json.Encode
import Json.Decode exposing ((:=))

import ServerSide.Markdown exposing (..)
import ServerSide.Constants exposing (..)
import ServerSide.Helpers exposing (..)


type NodeType
    = TextTag TextTagRecord
    | NodeEntry NodeRecord
    | CustomNode CustomNodeRecord
    | MarkdownNode MarkdownNodeRecord
    | NoOp


type alias TextTagRecord =
    { text : String }


type alias NodeRecord =
    { tag : String
    , children : List NodeType
    , facts :
        Facts
        --, namespace : String
    , descendantsCount : Int
    }


type alias MarkdownNodeRecord =
    { facts : Facts
    , model : MarkdownModel
    }

type alias CustomNodeRecord =
    { facts : Facts
    , model : Json.Decode.Value
    }


type alias Facts =
    { styles : Dict String String
    , events : Maybe Json.Decode.Value
    , attributes : Maybe Json.Decode.Value
    , attributeNamespace : Maybe Json.Decode.Value
    , stringOthers : Dict String String
    , boolOthers : Dict String Bool
    }


decodeNodeType : Json.Decode.Decoder NodeType
decodeNodeType =
    ("type" := Json.Decode.string)
        |> (flip Json.Decode.andThen)
            (\typeString ->
                case typeString of
                    "text" ->
                        Json.Decode.map TextTag decodeTextTag

                    "keyed-node" ->
                        Json.Decode.map NodeEntry decodeKeyedNode

                    "node" ->
                        Json.Decode.map NodeEntry decodeNode

                    "custom" ->
                        decodeCustomNode

                    "tagger" ->
                        decodeTagger

                    _ ->
                        Json.Decode.fail ("No such type as " ++ typeString)
            )


decodeTextTag : Json.Decode.Decoder TextTagRecord
decodeTextTag =
    ("text" := Json.Decode.customDecoder Json.Decode.string (\text -> Ok { text = text }))


encodeTextTag : TextTagRecord -> Json.Encode.Value
encodeTextTag { text } =
    Json.Encode.object [ ( "text", Json.Encode.string text ) ]


decodeTagger : Json.Decode.Decoder NodeType
decodeTagger =
    Json.Decode.oneOf
        [ Json.Decode.at [ "node" ] decodeNodeType
        , Json.Decode.at [ "text" ] decodeNodeType
        , Json.Decode.at [ "custom" ] decodeNodeType
        ]


decodeKeyedNode : Json.Decode.Decoder NodeRecord
decodeKeyedNode =
    Json.Decode.object4 NodeRecord
        ("tag" := Json.Decode.string)
        ("children" := Json.Decode.list (Json.Decode.at [ "_1" ] decodeNodeType))
        ("facts" := decodeFacts)
        ("descendantsCount" := Json.Decode.int)


decodeNode : Json.Decode.Decoder NodeRecord
decodeNode =
    Json.Decode.object4 NodeRecord
        ("tag" := Json.Decode.string)
        ("children" := Json.Decode.list decodeNodeType)
        ("facts" := decodeFacts)
        ("descendantsCount" := Json.Decode.int)


encodeNodeRecord : NodeRecord -> Json.Encode.Value
encodeNodeRecord record =
    Json.Encode.object
        [ ( "tag", Json.Encode.string record.tag )
          --, ( "children", Json.Encode.list encodeNodeType)
          --, ( "facts", encodeFacts)
        , ( "descendantsCount", Json.Encode.int record.descendantsCount )
        ]

decodeCustomNode : Json.Decode.Decoder NodeType
decodeCustomNode =
    Json.Decode.oneOf
        [ Json.Decode.map MarkdownNode decodeMarkdownNodeRecord
        , Json.Decode.map CustomNode decodeCustomNodeRecord
        ]


decodeCustomNodeRecord : Json.Decode.Decoder CustomNodeRecord
decodeCustomNodeRecord =
    Json.Decode.object2 CustomNodeRecord
        ("facts" := decodeFacts)
        ("model" := Json.Decode.value)


decodeMarkdownNodeRecord : Json.Decode.Decoder MarkdownNodeRecord
decodeMarkdownNodeRecord =
    Json.Decode.object2 MarkdownNodeRecord
        ("facts" := decodeFacts)
        ("model" := decodeMarkdownModel)


decodeStyles : Json.Decode.Decoder (Dict String String)
decodeStyles =
    Json.Decode.oneOf
        [ (styleKey := Json.Decode.dict Json.Decode.string)
        , Json.Decode.succeed Dict.empty
        ]


encodeStyles : Dict String String -> Json.Encode.Value
encodeStyles stylesDict =
    let
        encodedDict =
            stylesDict
                |> Dict.toList
                |> List.map (\( k, v ) -> ( k, Json.Encode.string v ))
    in
        Json.Encode.object [ ( styleKey, Json.Encode.object encodedDict ) ]


decodeOthers : Json.Decode.Decoder a -> Json.Decode.Decoder (Dict String a)
decodeOthers otherDecoder =
    Json.Decode.customDecoder (Json.Decode.dict Json.Decode.value)
        (filterKnownKeys
            >> Dict.toList
            >> List.filterMap
                (\( key, value ) ->
                    case Json.Decode.decodeValue otherDecoder value of
                        Err _ ->
                            Nothing

                        Ok v ->
                            Just ( key, v )
                )
            >> Dict.fromList
            >> Ok
        )


decodeFacts : Json.Decode.Decoder Facts
decodeFacts =
    Json.Decode.object6 Facts
        (decodeStyles)
        (Json.Decode.maybe (eventKey := Json.Decode.value))
        (Json.Decode.maybe (attributeKey := Json.Decode.value))
        (Json.Decode.maybe (attributeNamespaceKey := Json.Decode.value))
        (decodeOthers Json.Decode.string)
        (decodeOthers Json.Decode.bool)
