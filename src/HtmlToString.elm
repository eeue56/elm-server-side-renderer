module HtmlToString exposing (..)

import Html exposing (Html)
import String
import Dict exposing (Dict)
import Json.Decode
import ServerSide.Constants exposing (..)
import ServerSide.Helpers exposing (..)
import ServerSide.InternalTypes exposing (..)


emptyFacts : Facts
emptyFacts =
    { styles = Dict.empty
    , events = Nothing
    , attributeNamespace = Nothing
    , stringOthers = Dict.empty
    , boolOthers = Dict.empty
    }


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
nodeRecordToString { tag, children, facts } =
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
                        [] ->
                            ""

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
                [] ->
                    Nothing

                styles ->
                    styles
                        |> List.map (\( key, value ) -> key ++ ":" ++ value)
                        |> String.join ""
                        |> (\styleString -> "style=\"" ++ styleString ++ "\"")
                        |> Just

        classes =
            Dict.get "className" facts.stringOthers
                |> Maybe.map (\name -> "class=\"" ++ name ++ "\"")

        stringOthers =
            Dict.filter (\k v -> k /= "className") facts.stringOthers
                |> Dict.toList
                |> List.map (\( k, v ) -> k ++ "=\"" ++ v ++ "\"")
                |> String.join " "
                |> Just

        boolOthers =
            Dict.toList facts.boolOthers
                |> List.map (\( k, v ) -> k ++ "=" ++ (String.toLower <| toString v))
                |> String.join " "
                |> Just

    in
        String.join ""
            [ openTag [ classes, styles, stringOthers, boolOthers ]
            , childrenStrings
            , closeTag
            ]


{-| Convert a given html node to a string based on the type
-}
nodeTypeToString : NodeType -> String
nodeTypeToString nodeType =
    case nodeType of
        TextTag { text } ->
            text

        NodeEntry record ->
            nodeRecordToString record

        CustomNode record ->
            ""

        MarkdownNode record ->
            record.model.markdown

        NoOp ->
            ""


{-| Take a Html element, convert it to a string
Useful for tests
-}
htmlToString : Html msg -> String
htmlToString =
    nodeTypeFromHtml >> nodeTypeToString
