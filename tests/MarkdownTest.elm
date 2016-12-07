module MarkdownTest exposing (..)

import HtmlToString exposing (..)
import ServerSide.InternalTypes exposing (..)
import ServerSide.Helpers exposing (..)
import ServerSide.Markdown exposing (..)
import HtmlQuery exposing (..)
import Legacy.ElmTest as ElmTest exposing (..)
import Html
import Html.Attributes exposing (id)
import Markdown
import Dict


assertEqualPair : ( a, a ) -> Assertion
assertEqualPair ( left, right ) =
    assertEqual left right


emptyBlock : Html.Html msg
emptyBlock =
    Markdown.toHtml [] ""


emptyBlockAsString : String
emptyBlockAsString =
    ""


emptyBlockDecoded : NodeType
emptyBlockDecoded =
    MarkdownNode
        { facts = emptyFacts
        , model = baseMarkdownModel
        }


fullBlock : Html.Html msg
fullBlock =
    Markdown.toHtml [] fullBlockAsString


fullBlockAsString : String
fullBlockAsString =
    """
# hello
- one
- two
three!
"""


fullBlockDecoded : NodeType
fullBlockDecoded =
    MarkdownNode
        { facts = emptyFacts
        , model =
            { baseMarkdownModel
                | markdown = fullBlockAsString
            }
        }


fullBlockWithAttrs : Html.Html msg
fullBlockWithAttrs =
    Markdown.toHtml [ id "noah" ] fullBlockAsString


fullBlockWithAttrsDecoded : NodeType
fullBlockWithAttrsDecoded =
    MarkdownNode
        { facts =
            { emptyFacts
                | stringOthers = Dict.fromList [ ( "id", "noah" ) ]
            }
        , model =
            { baseMarkdownModel
                | markdown = fullBlockAsString
            }
        }


nodeTests : Test
nodeTests =
    suite "Node tests"
        [ test "empty markdown are empty as a string" <|
            assertEqualPair ( emptyBlockAsString, htmlToString emptyBlock )
        , test "empty markdown are decoded to empty custom nodes" <|
            assertEqualPair ( emptyBlockDecoded, nodeTypeFromHtml emptyBlock )
        , test "full markdown are full as a string" <|
            assertEqualPair ( fullBlockAsString, htmlToString fullBlock )
        , test "full markdown are decoded to full custom nodes" <|
            assertEqualPair ( fullBlockDecoded, nodeTypeFromHtml fullBlock )
        , test "attributes have no effect on the model" <|
            assertEqualPair ( fullBlockAsString, htmlToString fullBlockWithAttrs )
        , test "markdown preserves attributes as facts" <|
            assertEqualPair ( fullBlockWithAttrsDecoded, nodeTypeFromHtml fullBlockWithAttrs )
        ]
