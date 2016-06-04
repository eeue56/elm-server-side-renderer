module Query
    exposing
        ( query
        , queryById
        , queryByClassname
        , queryByTagname
        , queryByAttribute
        , queryInNode
        , Selector(..)
        )

import Html exposing (Html)
import HtmlToString exposing (nodeTypeFromHtml)
import InternalTypes exposing (..)
import Dict
import String


{-| Selectors to query a Html element
-}
type Selector
    = Id String
    | Classname String
    | Tag String
    | Attribute String String


{-| Query for a node with a given tag in a Html element
-}
queryByTagname : String -> Html msg -> List NodeType
queryByTagname tagname =
    query (Tag tagname)


{-| Query for a node with a given id in a Html element
-}
queryById : String -> Html msg -> List NodeType
queryById id =
    query (Id id)


{-| Query for a node with a given classname in a Html element
-}
queryByClassname : String -> Html msg -> List NodeType
queryByClassname classname =
    query (Classname classname)


{-| Query for a node with a given attribute in a Html element
-}
queryByAttribute : String -> String -> Html msg -> List NodeType
queryByAttribute key value =
    query (Attribute key value)


{-| Query a Html element using a selector
-}
query : Selector -> Html msg -> List NodeType
query selector =
    nodeTypeFromHtml >> queryInNode selector


{-| Query a Html node using a selector
-}
queryInNode : Selector -> NodeType -> List NodeType
queryInNode selector node =
    case node of
        NodeEntry record ->
            let
                mapChildren children =
                    List.concatMap (queryInNode selector) children

                predicate =
                    predicateFromSelector selector
            in
                if predicate record then
                    [ node ] ++ (mapChildren record.children)
                else
                    mapChildren record.children

        _ ->
            []


predicateFromSelector : Selector -> (NodeRecord -> Bool)
predicateFromSelector selector =
    case selector of
        Id id ->
            hasAttribute "id" id

        Classname classname ->
            hasClass classname

        Tag tag ->
            (==) tag << .tag

        Attribute key value ->
            hasAttribute key value


hasAttribute : String -> String -> NodeRecord -> Bool
hasAttribute attribute query { facts } =
    case Dict.get attribute facts.stringOthers of
        Just id ->
            id == query

        Nothing ->
            False


hasClass : String -> NodeRecord -> Bool
hasClass query { facts } =
    Dict.get "className" facts.stringOthers
        |> Maybe.withDefault ""
        |> String.split " "
        |> List.member query
