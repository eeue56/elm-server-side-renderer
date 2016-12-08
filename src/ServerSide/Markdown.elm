module ServerSide.Markdown exposing (..)

import Json.Encode
import Json.Decode


baseMarkdownModel : MarkdownModel
baseMarkdownModel =
    { options =
        { githubFlavored = Just { tables = False, breaks = False }
        , defaultHighlighting = Nothing
        , sanitize = False
        , smartypants = False
        }
    , markdown = ""
    }


type alias MarkdownOptions =
    { githubFlavored : Maybe { tables : Bool, breaks : Bool }
    , defaultHighlighting : Maybe String
    , sanitize : Bool
    , smartypants : Bool
    }


type alias MarkdownModel =
    { options : MarkdownOptions
    , markdown : String
    }


{-| We don't really care about encoding options right now
TODO: we will if we want to represent things as we do for elm-html
-}
encodeOptions : MarkdownOptions -> Json.Decode.Value
encodeOptions options =
    Json.Encode.null


encodeMarkdownModel : MarkdownModel -> Json.Decode.Value
encodeMarkdownModel model =
    Json.Encode.object
        [ ( "options", encodeOptions model.options )
        , ( "markdown", Json.Encode.string model.markdown )
        ]


decodeMarkdownModel : Json.Decode.Decoder MarkdownModel
decodeMarkdownModel =
    Json.Decode.map (MarkdownModel baseMarkdownModel.options)
        (Json.Decode.field "markdown" Json.Decode.string)
