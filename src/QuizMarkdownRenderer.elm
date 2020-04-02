module QuizMarkdownRenderer exposing (renderer)

import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font as Font
import Element.Input
import Element.Region
import Html
import Html.Attributes as Attr
import Markdown
import Markdown.Block as Markdown
import Markdown.Html
import Markdown.Parser as Markdown
import Markdown.Renderer as Markdown


renderer : Markdown.Renderer (Element msg)
renderer =
    { html = Markdown.Html.oneOf []
    , heading = heading
    , paragraph =
        Element.paragraph
            [ Element.spacing 15 ]
    , thematicBreak = Element.none
    , text = Element.text
    , strong = \content -> Element.row [ Font.bold ] content
    , emphasis = \content -> Element.row [ Font.italic ] content
    , codeSpan = code
    , link =
        \{ title, destination } body ->
            Element.newTabLink
                [ Element.htmlAttribute (Attr.style "display" "inline-flex") ]
                { url = destination
                , label =
                    Element.paragraph
                        [ Font.color (Element.rgb255 0 0 255)
                        ]
                        body
                }
    , hardLineBreak = Html.br [] [] |> Element.html
    , image =
        \image ->
            case image.title of
                Just title ->
                    Element.image [ Element.width Element.fill ] { src = image.src, description = image.alt }

                Nothing ->
                    Element.image [ Element.width Element.fill ] { src = image.src, description = image.alt }
    , blockQuote =
        \children ->
            Element.column
                [ Element.Border.widthEach { top = 0, right = 0, bottom = 0, left = 10 }
                , Element.padding 10
                , Element.Border.color (Element.rgb255 145 145 145)
                , Element.Background.color (Element.rgb255 245 245 245)
                ]
                children
    , unorderedList =
        \items ->
            Element.column [ Element.spacing 15 ]
                (items
                    |> List.map
                        (\(Markdown.ListItem task children) ->
                            Element.row [ Element.spacing 5 ]
                                [ Element.row
                                    [ Element.alignTop ]
                                    ((case task of
                                        Markdown.IncompleteTask ->
                                            Element.Input.defaultCheckbox False

                                        Markdown.CompletedTask ->
                                            Element.Input.defaultCheckbox True

                                        Markdown.NoTask ->
                                            Element.text "•"
                                     )
                                        :: Element.text " "
                                        :: children
                                    )
                                ]
                        )
                )
    , orderedList =
        \startingIndex items ->
            Element.column [ Element.spacing 15 ]
                (items
                    |> List.indexedMap
                        (\index itemBlocks ->
                            Element.row [ Element.spacing 5 ]
                                [ Element.row [ Element.alignTop ]
                                    (Element.text (String.fromInt (index + startingIndex) ++ " ") :: itemBlocks)
                                ]
                        )
                )
    , codeBlock = codeBlock
    , table = Element.column []
    , tableHeader = Element.column []
    , tableBody = Element.column []
    , tableRow = Element.row []
    , tableHeaderCell =
        \maybeAlignment children ->
            Element.paragraph [] children
    , tableCell = Element.paragraph []
    }


heading : { level : Markdown.HeadingLevel, rawText : String, children : List (Element msg) } -> Element msg
heading { level, rawText, children } =
    let
        rawTextToId =
            String.split " "
                >> String.join "-"
                >> String.toLower
    in
    Element.paragraph
        [ Font.size
            (case level of
                Markdown.H1 ->
                    36

                Markdown.H2 ->
                    24

                _ ->
                    20
            )
        , Font.bold
        , Font.family [ Font.typeface "Montserrat" ]
        , Element.Region.heading (Markdown.headingLevelToInt level)
        , Element.htmlAttribute
            (Attr.attribute "name" (rawTextToId rawText))
        , Element.htmlAttribute
            (Attr.id (rawTextToId rawText))
        ]
        children


code : String -> Element msg
code snippet =
    Element.el
        [ Element.Background.color
            (Element.rgba 0 0 0 0.04)
        , Element.Border.rounded 2
        , Element.paddingXY 5 3
        , Font.family
            [ Font.external
                { url = "https://fonts.googleapis.com/css?family=Source+Code+Pro"
                , name = "Source Code Pro"
                }
            ]
        ]
        (Element.text snippet)


codeBlock : { body : String, language : Maybe String } -> Element msg
codeBlock details =
    Element.el
        [ Element.Background.color (Element.rgba 0 0 0 0.03)
        , Element.htmlAttribute (Attr.style "white-space" "pre")
        , Element.padding 20
        , Font.family
            [ Font.external
                { url = "https://fonts.googleapis.com/css?family=Source+Code+Pro"
                , name = "Source Code Pro"
                }
            ]
        ]
        (Element.text details.body)
