module HtmlToString exposing (..) -- where

import Html exposing (Html)

import Json.Decode exposing ((:=))
import String
import Dict exposing (Dict)

import Constants exposing (..)
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
    , events : Maybe Json.Decode.Value
    , attributes : Maybe Json.Decode.Value
    , attributeNamespace : Maybe Json.Decode.Value
    , others : Dict String String
    }

emptyFacts : Facts
emptyFacts =
    { styles = Dict.empty
    , events = Nothing
    , attributes = Nothing
    , attributeNamespace = Nothing
    , others = Dict.empty
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
    Json.Decode.object4 NodeRecord
        ( "tag" := Json.Decode.string )
        ( "children" := Json.Decode.list decodeNodeType)
        ( "facts" := decodeFacts)
        ( "descendantsCount" := Json.Decode.int )

decodeStyles : Json.Decode.Decoder (Dict String String)
decodeStyles =
    Json.Decode.oneOf
        [ ( styleKey := Json.Decode.dict Json.Decode.string )
        , Json.Decode.succeed Dict.empty
        ]

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
        ( decodeStyles )
        ( Json.Decode.maybe ( eventKey :=  Json.Decode.value ) )
        ( Json.Decode.maybe ( attributeKey := Json.Decode.value ) )
        ( Json.Decode.maybe ( attributeNamespaceKey := Json.Decode.value ) )
        ( decodeOthers )

{-| Convert a generic Html msg to a given NodeType. If we fail to parse,
fall back on a NoOp node type.
-}
nodeTypeFromHtml : Html msg -> NodeType
nodeTypeFromHtml =
    stringify
        >> Json.Decode.decodeString decodeNodeType
        >> Result.withDefault NoOp

{-| Convert a node record to a string. This basically takes the tag name, then
    pulls all the facts into tag declaration, then goes through the children and
    nests them undert hsi one
-}
nodeRecordToString : NodeRecord -> String
nodeRecordToString {tag, children, facts} =
    let
        openTag : List (Maybe String) -> String
        openTag extras =
            let
                trimmedExtras =
                    List.filterMap (\x -> x) extras
                        |> List.map String.trim
                        |> List.filter ((/=) "")

                filling =
                    case trimmedExtras of
                        [] -> ""
                        more ->
                            " " ++ (String.join " " more)
            in
                "<" ++ tag ++ filling ++ ">"

        closeTag =
            "</" ++ tag ++ ">"

        childrenStrings =
            List.map nodeTypeToString children
                |> String.join ""

        styles =
            case Dict.toList facts.styles of
                [] -> Nothing
                styles ->
                    styles
                        |> List.map (\(key, value) -> key ++ ":" ++ value)
                        |> String.join ""
                        |> (\styleString -> "style=\"" ++ styleString ++ "\"")
                        |> Just

        classes =
            Dict.get "className" facts.others
                |> Maybe.map (\name -> "class=\"" ++ name ++ "\"")

    in
        String.join ""
            [ openTag [ classes, styles ]
            , childrenStrings
            , closeTag
            ]

{-| Convert a given html node to a string based on the type
-}
nodeTypeToString : NodeType -> String
nodeTypeToString nodeType =
    case nodeType of
        TextTag {text} ->
            text
        NodeEntry record ->
            nodeRecordToString record
        NoOp ->
            ""

{-| Take a Html element, convert it to a string
Useful for tests
-}
htmlToString : Html msg -> String
htmlToString  =
    nodeTypeFromHtml >> nodeTypeToString
