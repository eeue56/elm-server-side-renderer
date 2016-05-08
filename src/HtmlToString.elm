module HtmlToString exposing (..) -- where

import Html exposing (Html)
import Json.Decode exposing ((:=))
import String
import Dict exposing (Dict)
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
    , facts : Facts
    --, namespace : String
    , descendantsCount : Int
    }

type alias Facts =
    { styles : Dict String String
    , events : Json.Decode.Value
    , attributes : Json.Decode.Value
    , attributeNamespace : Json.Decode.Value
    , others : Dict String String
    }

styleKey : String
styleKey = "STYLE"
eventKey = "EVENT"
attributeKey = "ATTR"
attributeNamespaceKey = "ATTR_NS"

knownKeys = [ styleKey, eventKey, attributeKey, attributeNamespaceKey ]

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
    Json.Decode.object4 NodeRecord
        ( "tag" := Json.Decode.string )
        ( "children" := Json.Decode.list decodeNodeType)
        ( "facts" := decodeFacts)
        ( "descendantsCount" := Json.Decode.int )

decodeStyles : Json.Decode.Decoder (Dict String String)
decodeStyles =
    Json.Decode.dict Json.Decode.string

filterKnownKeys : Dict String a -> Dict String a
filterKnownKeys =
    Dict.filter (\key _ -> List.member key knownKeys)


decodeOthers : Json.Decode.Decoder (Dict String String)
decodeOthers =
    Json.Decode.customDecoder
        (Json.Decode.dict Json.Decode.value)
        (filterKnownKeys
            >> Dict.toList
            >> List.filterMap (\(key, value) ->
                case Json.Decode.decodeValue Json.Decode.string value of
                    Err _ ->
                        Nothing
                    Ok v ->
                        Just (key, v)
                )
            >> Dict.fromList
            >> Ok
        )

decodeFacts : Json.Decode.Decoder Facts
decodeFacts =
    Json.Decode.object5 Facts
        ( styleKey := decodeStyles )
        ( eventKey := Json.Decode.value )
        ( attributeKey := Json.Decode.value )
        ( attributeNamespaceKey := Json.Decode.value )
        ( decodeOthers )



nodeTypeFromHtml : Html msg -> NodeType
nodeTypeFromHtml =
    stringify
        >> Debug.log "hm"
        >> Json.Decode.decodeString decodeNodeType
        >> Result.withDefault NoOp


nodeRecordToString : NodeRecord -> String
nodeRecordToString {tag, children, facts} =
    let
        openTag =
            "<" ++ tag ++ classes ++ ">"

        closeTag =
            "</" ++ tag ++ ">"

        childrenStrings =
            List.map nodeTypeToString children
                |> String.join ""

        classes =
            Dict.get "className" facts.others
                |> Maybe.map (\name -> " class=\"" ++ name ++ "\"")
                |> Maybe.withDefault ""

    in
        String.join ""
            [ openTag
            , childrenStrings
            , closeTag
            ]

nodeTypeToString : NodeType -> String
nodeTypeToString nodeType =
    case nodeType of
        TextTag {text} ->
            text
        NodeEntry record ->
            nodeRecordToString record
        NoOp ->
            ""


htmlToString : Html msg -> String
htmlToString  =
    nodeTypeFromHtml >> nodeTypeToString
