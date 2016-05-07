module Helpers exposing (..) -- where

import String
import Regex
import Native.Helpers


stringify : a -> String
stringify =
    Native.Helpers.stringify

wrapper : String -> String -> String
wrapper left right =
    "\"" ++ (String.trim left) ++ "\"" ++ ":" ++ right

replaceInternalStructure : String -> String
replaceInternalStructure =
    Regex.replace Regex.All (Regex.regex "<internal structure>") (\_ -> "null")

replaceChildren : String -> String
replaceChildren =
    Native.Helpers.replaceChildren

wrapFieldsWithQuotes : String -> String
wrapFieldsWithQuotes =
    let
        rejoiner parts =
            case parts of
                left::right::_ ->
                    if String.startsWith "{" left then
                        "{" ++ (wrapper (String.dropLeft 1 left) right)
                    else
                        wrapper left right
                _ ->
                    String.join ":" parts
    in
        String.split ","
            >> List.map (String.split ":" >> Debug.log "parts" >> rejoiner)
            >> String.join ","
