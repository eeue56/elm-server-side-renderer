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

import Dict exposing (Dict)
import Json.Encode
import Json.Decode
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
    , attributeNamespace : Maybe Json.Decode.Value
    , stringOthers : Dict String String
    , boolOthers : Dict String Bool
    }


decodeNodeType : Json.Decode.Decoder NodeType
decodeNodeType =
    Json.Decode.field "type" Json.Decode.string
        |> Json.Decode.andThen
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
    Json.Decode.field "text"
        (Json.Decode.map (\text -> { text = text }) Json.Decode.string)


encodeTextTag : TextTagRecord -> Json.Encode.Value
encodeTextTag { text } =
    Json.Encode.object [ ( "text", Json.Encode.string text ) ]


decodeTagger : Json.Decode.Decoder NodeType
decodeTagger =
    Json.Decode.oneOf
        [ Json.Decode.field "node" decodeNodeType
        , Json.Decode.field "text" decodeNodeType
        , Json.Decode.field "custom" decodeNodeType
        ]


decodeKeyedNode : Json.Decode.Decoder NodeRecord
decodeKeyedNode =
    let
        -- elm stores keyed nodes as tuples
        -- we only want to decode the html, in the second property
        decodeSecondNode =
            Json.Decode.field "_1" decodeNodeType
    in
        Json.Decode.map4 NodeRecord
            (Json.Decode.field "tag" Json.Decode.string)
            (Json.Decode.field "children" (Json.Decode.list decodeSecondNode))
            (Json.Decode.field "facts" decodeFacts)
            (Json.Decode.field "descendantsCount" Json.Decode.int)


decodeNode : Json.Decode.Decoder NodeRecord
decodeNode =
    Json.Decode.map4 NodeRecord
        (Json.Decode.field "tag" Json.Decode.string)
        (Json.Decode.field "children" (Json.Decode.list decodeNodeType))
        (Json.Decode.field "facts" decodeFacts)
        (Json.Decode.field "descendantsCount" Json.Decode.int)


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
    Json.Decode.map2 CustomNodeRecord
        (Json.Decode.field "facts" decodeFacts)
        (Json.Decode.field "model" Json.Decode.value)


decodeMarkdownNodeRecord : Json.Decode.Decoder MarkdownNodeRecord
decodeMarkdownNodeRecord =
    Json.Decode.map2 MarkdownNodeRecord
        (Json.Decode.field "facts" decodeFacts)
        (Json.Decode.field "model" decodeMarkdownModel)


decodeStyles : Json.Decode.Decoder (Dict String String)
decodeStyles =
    Json.Decode.oneOf
        [ Json.Decode.field styleKey (Json.Decode.dict Json.Decode.string)
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

{-| grab things from attributes via a decoder, then anything that isn't filtered on
    the object
-}
decodeOthers : Json.Decode.Decoder a -> Json.Decode.Decoder (Dict String a)
decodeOthers otherDecoder =
    decodeAttributes otherDecoder
        |> Json.Decode.andThen (\attributes ->
            decodeDictFilterMap otherDecoder
                |> Json.Decode.map (filterKnownKeys >> Dict.union attributes)
        )

{-| For a given decoder, keep the values from a dict that pass the decoder -}
decodeDictFilterMap : Json.Decode.Decoder a -> Json.Decode.Decoder (Dict String a)
decodeDictFilterMap decoder =
    Json.Decode.dict Json.Decode.value
        |> Json.Decode.map
            (Dict.toList
                >> List.filterMap
                    (\(key, value) ->
                        case Json.Decode.decodeValue decoder value of
                            Err _ -> Nothing
                            Ok v -> Just ( key, v )
                    )
                >> Dict.fromList
            )

decodeAttributes : Json.Decode.Decoder a -> Json.Decode.Decoder (Dict String a)
decodeAttributes decoder =
    Json.Decode.oneOf
        [ Json.Decode.field attributeKey (decodeDictFilterMap decoder)
        , Json.Decode.succeed Dict.empty
        ]


decodeFacts : Json.Decode.Decoder Facts
decodeFacts =
    Json.Decode.map5 Facts
        (decodeStyles)
        (Json.Decode.maybe (Json.Decode.field eventKey Json.Decode.value))
        (Json.Decode.maybe (Json.Decode.field attributeNamespaceKey Json.Decode.value))
        (decodeOthers Json.Decode.string)
        (decodeOthers Json.Decode.bool)
