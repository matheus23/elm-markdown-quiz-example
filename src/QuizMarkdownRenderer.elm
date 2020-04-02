module QuizMarkdownRenderer exposing (render)

{-| This module was basically copied from elm-markdowns example directory, see the
ElmUi.elm file.

If you're not using elm-ui, just copy the source code of the
`Markdown.Renderer.defaultHtmlRenderer` instead.

-}

import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Events
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
import Set
import Types exposing (..)


type alias Context =
    { model : Model
    , revealed : Bool
    , answerId : Maybe String
    }


render : List Markdown.Block -> Result String (Model -> Element Msg)
render blocks =
    blocks
        |> Markdown.render renderer
        |> Result.map
            (\blockViews model ->
                let
                    initialContext =
                        { model = model, revealed = True, answerId = Nothing }
                in
                blockViews
                    |> renderAll initialContext
                    |> Element.paragraph [ Element.spacing 8 ]
            )


renderer : Markdown.Renderer (Context -> Element Msg)
renderer =
    { html =
        Markdown.Html.oneOf
            [ Markdown.Html.tag "answers"
                (\id children context ->
                    let
                        innerContext =
                            { context
                                | revealed = Set.member id context.model.revealedQuizzes
                                , answerId = Just id
                            }
                    in
                    Element.paragraph
                        [ Element.htmlAttribute (Attr.id id) ]
                        (renderAll innerContext children)
                )
                |> Markdown.Html.withAttribute "id"
            ]
    , heading = heading
    , paragraph =
        \children context ->
            Element.paragraph
                [ Element.spacing 15 ]
                (renderAll context children)
    , thematicBreak = \_ -> Element.none
    , text = \text _ -> Element.text text
    , strong = \content context -> Element.row [ Font.bold ] (renderAll context content)
    , emphasis = \content context -> Element.row [ Font.italic ] (renderAll context content)
    , codeSpan = code
    , link =
        \{ title, destination } body context ->
            Element.newTabLink
                [ Element.htmlAttribute (Attr.style "display" "inline-flex") ]
                { url = destination
                , label =
                    Element.paragraph
                        [ Font.color (Element.rgb255 0 0 255)
                        ]
                        (renderAll context body)
                }
    , hardLineBreak = \_ -> Html.br [] [] |> Element.html
    , image =
        \image _ ->
            case image.title of
                Just title ->
                    Element.image [ Element.width Element.fill ] { src = image.src, description = image.alt }

                Nothing ->
                    Element.image [ Element.width Element.fill ] { src = image.src, description = image.alt }
    , blockQuote =
        \children context ->
            Element.column
                [ Element.Border.widthEach { top = 0, right = 0, bottom = 0, left = 10 }
                , Element.padding 10
                , Element.Border.color (Element.rgb255 145 145 145)
                , Element.Background.color (Element.rgb255 245 245 245)
                ]
                (renderAll context children)
    , unorderedList =
        \items context ->
            let
                events =
                    context.answerId
                        |> Maybe.map (List.singleton << Element.Events.onClick << RevealQuiz)
                        |> Maybe.withDefault []
            in
            Element.column [ Element.spacing 15 ]
                (items
                    |> List.map
                        (\(Markdown.ListItem task children) ->
                            Element.row [ Element.spacing 5 ]
                                [ Element.row
                                    (Element.alignTop :: events)
                                    ((case task of
                                        Markdown.IncompleteTask ->
                                            Element.Input.defaultCheckbox False

                                        Markdown.CompletedTask ->
                                            Element.Input.defaultCheckbox (True && context.revealed)

                                        Markdown.NoTask ->
                                            Element.text "•"
                                     )
                                        :: Element.text " "
                                        :: renderAll context children
                                    )
                                ]
                        )
                )
    , orderedList =
        \startingIndex items context ->
            Element.column [ Element.spacing 15 ]
                (items
                    |> List.indexedMap
                        (\index itemBlocks ->
                            Element.row [ Element.spacing 5 ]
                                [ Element.row [ Element.alignTop ]
                                    (Element.text (String.fromInt (index + startingIndex) ++ " ") :: renderAll context itemBlocks)
                                ]
                        )
                )
    , codeBlock = codeBlock
    , table = \children context -> Element.column [] (renderAll context children)
    , tableHeader = \children context -> Element.column [] (renderAll context children)
    , tableBody = \children context -> Element.column [] (renderAll context children)
    , tableRow = \children context -> Element.row [] (renderAll context children)
    , tableHeaderCell =
        \maybeAlignment children context ->
            Element.paragraph [] (renderAll context children)
    , tableCell = \children context -> Element.paragraph [] (renderAll context children)
    }


renderAll : context -> List (context -> view) -> List view
renderAll context =
    List.map ((|>) context)


heading : { level : Markdown.HeadingLevel, rawText : String, children : List (Context -> Element msg) } -> Context -> Element msg
heading { level, rawText, children } context =
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
        (renderAll context children)


code : String -> Context -> Element msg
code snippet _ =
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


codeBlock : { body : String, language : Maybe String } -> Context -> Element msg
codeBlock details _ =
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
