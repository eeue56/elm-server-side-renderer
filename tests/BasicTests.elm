module BasicTests exposing (..) -- where

import HtmlToString exposing (..)
import ElmTest exposing (..)
import Html
import Json.Decode


-- DATA

-- Empty things

-- { type = \"text\", text = \"\" }"
emptyText : String
emptyText =
    ("")

emptyTextDecoded : NodeType
emptyTextDecoded =
    TextTag { text = emptyText }



emptyDiv : Html.Html msg
emptyDiv =
    Html.div [] []

emptyDivAsString : String
emptyDivAsString =
    "<div></div>"

emptyDivDecoded : NodeType
emptyDivDecoded =
    NodeEntry { tag = "div", children = [], descendantsCount = 0 }


-- Non empty things!

nonEmptyText : String
nonEmptyText =
    ("hello")

nonEmptyTextDecoded : NodeType
nonEmptyTextDecoded =
    TextTag { text = nonEmptyText }


oneChildDiv : Html.Html msg
oneChildDiv =
    Html.div [] [ Html.text nonEmptyText ]

oneChildDivAsString : String
oneChildDivAsString =
    "<div>" ++ nonEmptyText ++ "</div>"

oneChildDivDecoded : NodeType
oneChildDivDecoded =
    NodeEntry { tag = "div", children = [ nonEmptyTextDecoded ], descendantsCount = 1 }

oneChildSpan : Html.Html msg
oneChildSpan =
    Html.span [] [ Html.text nonEmptyText ]

oneChildSpanAsString : String
oneChildSpanAsString =
    "<span>" ++ nonEmptyText ++ "</span>"

oneChildSpanDecoded : NodeType
oneChildSpanDecoded =
    NodeEntry { tag = "span", children = [ nonEmptyTextDecoded ], descendantsCount = 1 }


twoChildForm : Html.Html msg
twoChildForm =
    Html.form [] [ oneChildDiv, oneChildSpan ]

twoChildFormAsString : String
twoChildFormAsString =
    "<form>" ++ oneChildDivAsString ++ oneChildSpanAsString ++ "</form>"

countDescendents : NodeType -> Int
countDescendents nodeType =
    case nodeType of
        NodeEntry {descendantsCount} ->
            descendantsCount
        TextTag _ ->
            1
        _ ->
            0


twoChildFormDecoded : NodeType
twoChildFormDecoded =
    let
        children =
            [ oneChildDivDecoded
            , oneChildSpanDecoded
            ]
    in
        NodeEntry
            { tag = "form"
            , children = children
            , descendantsCount = 4
            }


-- HELPERS

textTagTypeFromString : String -> NodeType
textTagTypeFromString =
    Html.text >> nodeTypeFromHtml

textFromHtml : String -> String
textFromHtml =
    Html.text >> htmlToString

assertEqualPair : (a, a) -> Assertion
assertEqualPair (left, right) =
    assertEqual left right

textTests : Test
textTests =
    suite "Text tests"
        [ test "empty strings are empty results"
            <| assertEqualPair (emptyText, textFromHtml emptyText)
        , test "empty strings are decoded to empty text tags"
            <| assertEqualPair (emptyTextDecoded, textTagTypeFromString emptyText)
        , test "non empty strings are non empty results"
            <| assertEqualPair (nonEmptyText, textFromHtml nonEmptyText)
        , test "non strings are decoded to non text tags"
            <| assertEqualPair (nonEmptyTextDecoded, textTagTypeFromString nonEmptyText)
        ]

nodeTests : Test
nodeTests =
    suite "Node tests"
        [ test "empty divs are empty divs as a string"
            <| assertEqualPair (emptyDivAsString, htmlToString emptyDiv)
        , test "empty divs are decoded to empty div nodes"
            <| assertEqualPair (emptyDivDecoded, nodeTypeFromHtml emptyDiv)

        , test "divs with one non-empty text node are just a div with text"
            <| assertEqualPair (oneChildDivAsString, htmlToString oneChildDiv)
        , test "divs with one non-empty text node are decoded to just a div with text"
            <| assertEqualPair (oneChildDivDecoded, nodeTypeFromHtml oneChildDiv)

        , test "spans with one non-empty text node are just a span with text"
            <| assertEqualPair (oneChildSpanAsString, htmlToString oneChildSpan)
        , test "spans with one non-empty text node are decoded to just a span with text"
            <| assertEqualPair (oneChildSpanDecoded, nodeTypeFromHtml oneChildSpan)


        , test "forms with two non-empty text children are just a form with text"
            <| assertEqualPair (twoChildFormAsString, htmlToString twoChildForm)
        , test "forms with two non-empty text children are decoded to just a form with text"
            <| assertEqualPair (twoChildFormDecoded, nodeTypeFromHtml twoChildForm)
        ]

allTests : Test
allTests =
    suite "Html rendering"
        [ textTests
        , nodeTests
        ]

main : Program Never
main =
    runSuite allTests
