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
    Html.div []
        [ Html.button [ Html.Events.onClick LightSide ] [ Html.text "nooo" ]
        , Html.App.map SubSubComp r2d2
        ]


r2d2 : Html.Html SubSubMsg
r2d2 =
    Html.span [ Html.Events.onClick Beep ] []


type Msg
    = SubComp SubMsg


type SubMsg
    = LightSide
    | SubSubComp SubSubMsg


type SubSubMsg
    = Beep


all : Test
all =
    suite "tea tests"
        [ test "should render the parent view" <|
            assertEqual 1 <|
                List.length <|
                    queryByTagname "p" darthVader
        , test "should render child views" <|
            assertEqual 1 <|
                List.length <|
                    queryByTagname "button" darthVader
        , test "should render child views of child views" <|
            assertEqual 1 <|
                List.length <|
                    queryByTagname "span" darthVader
        ]
