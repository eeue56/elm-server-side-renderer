module Query exposing (..)

import Html exposing (Html)
import HtmlToString exposing (nodeTypeFromHtml)
import InternalTypes exposing (..)
import Dict
import String


{-| Query for a node with a given tag in a Html node
-}
queryTagnameInNode : String -> NodeType -> List NodeType
queryTagnameInNode tagname node =
    let
        mapChildren children =
            List.concatMap (queryTagnameInNode tagname) children
    in
        case node of
            NodeEntry record ->
                if record.tag == tagname then
                    [ node ] ++ (mapChildren record.children)
                else
                    mapChildren record.children

            _ ->
                []


{-| Query for a node with a given Fact in a Html node
-}
queryFactInNode : (Facts -> Bool) -> String -> NodeType -> List NodeType
queryFactInNode predicate query node =
    case node of
        NodeEntry record ->
            let
                mapChildren children =
                    List.concatMap (queryFactInNode predicate query) children

                found =
                    predicate record.facts
            in
                if found then
                    [ node ] ++ (mapChildren record.children)
                else
                    mapChildren record.children

        _ ->
            []


{-| Query for a node with a given classname in a Html node
-}
queryClassInNode : String -> NodeType -> List NodeType
queryClassInNode query node =
    let
        predicate { stringOthers } =
            Dict.get "className" stringOthers
                |> Maybe.withDefault ""
                |> String.split " "
                |> List.member query
    in
        queryFactInNode predicate query node


{-| Query for a node with a given id in a Html node
-}
queryIdInNode : String -> NodeType -> List NodeType
queryIdInNode query node =
    let
        predicate { stringOthers } =
            case Dict.get "id" stringOthers of
                Just id ->
                    id == query

                Nothing ->
                    False
    in
        queryFactInNode predicate query node


{-| Helper for query functions
-}
queryByHtml : (String -> NodeType -> List NodeType) -> String -> Html msg -> List NodeType
queryByHtml queryFn query html =
    let
        node =
            nodeTypeFromHtml html
    in
        queryFn query node


{-| Query for a node with a given tag in a Html element
-}
queryTagname : String -> Html msg -> List NodeType
queryTagname =
    queryByHtml queryTagnameInNode


{-| Query for a node with a given id in a Html element
-}
queryId : String -> Html msg -> List NodeType
queryId =
    queryByHtml queryIdInNode


{-| Query for a node with a given classname in a Html element
-}
queryClass : String -> Html msg -> List NodeType
queryClass =
    queryByHtml queryClassInNode
