module HtmlQuery
    exposing
        ( query
        , queryById
        , queryByClassname
        , queryByClassList
        , queryByTagname
        , queryByAttribute
        , queryByBoolAttribute
        , queryAll
        , queryInNode
        , Selector(..)
        )

import Dict
import String
import Html exposing (Html)
import HtmlToString exposing (nodeTypeFromHtml)
import ServerSide.InternalTypes exposing (..)


{-| Selectors to query a Html element
-}
type Selector
    = Id String
    | Classname String
    | ClassList (List String)
    | Tag String
    | Attribute String String
    | BoolAttribute String Bool
    | ContainsText String
    | Multiple (List Selector)


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


{-| Query for a node with all the given classnames in a Html element
-}
queryByClassList : List String -> Html msg -> List NodeType
queryByClassList classList =
    query (ClassList classList)


{-| Query for a node with a given attribute in a Html element
-}
queryByAttribute : String -> String -> Html msg -> List NodeType
queryByAttribute key value =
    query (Attribute key value)


{-| Query for a node with a given attribute in a Html element
-}
queryByBoolAttribute : String -> Bool -> Html msg -> List NodeType
queryByBoolAttribute key value =
    query (BoolAttribute key value)


{-| Query a Html element using a selector
-}
query : Selector -> Html msg -> List NodeType
query selector =
    nodeTypeFromHtml >> queryInNode selector


{-| Query to ensure a html node has all selectors given
-}
queryAll : List Selector -> Html msg -> List NodeType
queryAll selectors =
    query (Multiple selectors)


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

        TextTag { text } ->
            case selector of
                ContainsText innerText ->
                    if text == innerText then
                        [ node ]
                    else
                        []

                _ ->
                    []

        _ ->
            []


predicateFromSelector : Selector -> (NodeRecord -> Bool)
predicateFromSelector selector =
    case selector of
        Id id ->
            hasAttribute "id" id

        Classname classname ->
            hasClass classname

        ClassList classList ->
            hasClasses classList

        Tag tag ->
            (==) tag << .tag

        Attribute key value ->
            hasAttribute key value

        BoolAttribute key value ->
            hasBoolAttribute key value

        ContainsText text ->
            always False

        Multiple selectors ->
            hasAllSelectors selectors


hasAllSelectors : List Selector -> NodeRecord -> Bool
hasAllSelectors selectors record =
    List.map predicateFromSelector selectors
        |> List.map (\selector -> selector record)
        |> List.all identity


hasAttribute : String -> String -> NodeRecord -> Bool
hasAttribute attribute query { facts } =
    case Dict.get attribute facts.stringOthers of
        Just id ->
            id == query

        Nothing ->
            False


hasBoolAttribute : String -> Bool -> NodeRecord -> Bool
hasBoolAttribute attribute value { facts } =
    case Dict.get attribute facts.boolOthers of
        Just id ->
            id == value

        Nothing ->
            False


hasClass : String -> NodeRecord -> Bool
hasClass query record =
    List.member query (classnames record)


hasClasses : List String -> NodeRecord -> Bool
hasClasses classList record =
    containsAll classList (classnames record)


classnames : NodeRecord -> List String
classnames { facts } =
    Dict.get "className" facts.stringOthers
        |> Maybe.withDefault ""
        |> String.split " "


containsAll : List a -> List a -> Bool
containsAll a b =
    b
        |> List.foldl (\i acc -> List.filter ((/=) i) acc) a
        |> List.isEmpty
