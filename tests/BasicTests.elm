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
    NodeEntry { tag = "div" }


-- Non empty things!

nonEmptyText : String
nonEmptyText =
    ("hello")

nonEmptyTextDecoded : NodeType
nonEmptyTextDecoded =
    TextTag { text = nonEmptyText }



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
            <| assertEqualPair (emptyTextDecoded, nodeTypeFromHtml emptyDiv)
        ]

allTests : Test
allTests =
    suite "Html rendering"
        [ textTests
        , nodeTests
        ]

main : Program Never
main =
    runSuite textTests
