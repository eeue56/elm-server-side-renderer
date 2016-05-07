module Helpers exposing (..)

import String
import Regex

wrapFieldsWithQuotes : String -> String
wrapFieldsWithQuotes =
    let
        pattern =
            let
                beforeChars =
                    "[^{},]"
                leftChars =
                    "\\S*\\s+"
                splitters =
                    "[:=]+"
                right =
                    ".+?"
                afterChars =
                    "[ {}]"

                group xs =
                    "(" ++ xs ++ ")"
            in
                String.join ""
                    [ beforeChars
                    , group leftChars
                    , group <| String.join "" [splitters, right]
                    , afterChars
                    ]
                    |> Regex.regex

        replacer {match, submatches} =
            let
                left =
                    List.head submatches
                        |> Maybe.map (Maybe.withDefault "")
                        |> Maybe.withDefault ""
                        |> String.trim
                right =
                    List.drop 1 submatches
                        |> List.head
                        |> Maybe.map (Maybe.withDefault "")
                        |> Maybe.withDefault ""
            in
                "\"" ++ left ++ "\"" ++ right
    in
        Regex.replace Regex.All pattern replacer
