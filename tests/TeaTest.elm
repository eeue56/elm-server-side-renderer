module TeaTest exposing (..)

import ElmTest exposing (..)
import Html
import Html.App
import Html.Attributes
import Html.Events
import HtmlQuery exposing (..)
import HtmlToString exposing (..)


darthVader : Html.Html Msg
darthVader =
    Html.div []
        [ Html.p [] [ Html.text "Luke I'm your father." ]
        , Html.App.map SubComp lukeSkywalker
        ]


lukeSkywalker : Html.Html SubMsg
lukeSkywalker =
    Html.button [ Html.Events.onClick LightSide ] [ Html.text "nooo" ]


type Msg
    = SubComp SubMsg


type SubMsg
    = LightSide


all : Test
all =
    suite "tea tests"
        [ test "should render the parent view" <|
            assertEqual 1 <|
                List.length <|
                    queryByTagname "p" darthVader
        , test "should render the child view" <|
            assertEqual 1 <|
                List.length <|
                    queryByTagname "button" darthVader
        ]
