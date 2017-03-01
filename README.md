# elm-server-side-renderer


Take a `Html msg` element and turn it into a string.


```elm
> import HtmlToString exposing (htmlToString)
> import Html
> ourDiv = Html.div [ ] [ Html.text "hello world" ]
> htmlToString ourDiv
"<div>hello world</div>" : String


> import Html.Attributes exposing (class)
> ourDiv = Html.div [ class "donkey" ] [ Html.text "hello world" ]
> htmlToString ourDiv
"<div class=\"donkey\">hello world</div>" : String


> import Html.Attributes exposing (class, style)
> ourDiv = Html.div [ class "donkey", style [ ("color", "red") ] ] [ Html.text "hello world" ]
> htmlToString ourDiv
"<div class=\"donkey\" style=\"color:red\">hello world</div>" : String
```
