module Ui.View exposing (..)

import Core exposing (CodeFormat(..), Language(..), LanguageSelection(..), Model, Msg(..), RequestType(..), Response(..), langToLangCode, requestTypeToMimeType)
import Element exposing (Color, Element, alignRight, alignTop, centerX, centerY, column, el, fill, fromRgb255, height, html, htmlAttribute, inFront, none, padding, paddingXY, paragraph, pointer, px, rgb255, row, spacing, text, textColumn, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html as HT
import Html.Attributes as HA
import Http.Detailed
import Template exposing (Template, render, template, withString, withValue)


colourScheme :
    { white : Color
    , darkBlue : Color
    , translucentGrey : Color
    }
colourScheme =
    { white = rgb255 255 255 255
    , darkBlue = rgb255 29 53 87
    , translucentGrey =
        fromRgb255
            { alpha = 0.5
            , blue = 153
            , green = 136
            , red = 119
            }
    }


view : Model -> Element Msg
view model =
    let
        errView =
            case model.serverResponse of
                Error (Http.Detailed.BadStatus d m) ->
                    row
                        [ width fill
                        , height fill
                        , Background.color colourScheme.translucentGrey
                        , htmlAttribute (HA.attribute "style" "backdrop-filter: blur(3px); -webkit-backdrop-filter: blur(3px); z-index:200;")
                        ]
                        [ column
                            [ width (px 400)
                            , height (px 200)
                            , Background.color colourScheme.white
                            , centerY
                            , centerX
                            , padding 10
                            , Border.color (rgb255 0 0 0)
                            , Border.width 1
                            ]
                            [ row
                                [ width fill
                                , alignTop
                                ]
                                [ el
                                    [ Events.onClick UserClickedErrorMessageDismiss
                                    , alignRight
                                    , pointer
                                    ]
                                    (text "X")
                                ]
                            , row
                                [ width fill
                                , height fill
                                ]
                                [ textColumn
                                    [ width fill
                                    , height fill
                                    ]
                                    [ paragraph
                                        [ centerX
                                        , centerY
                                        , Font.bold
                                        ]
                                        [ text (String.fromInt d.statusCode ++ " " ++ d.statusText) ]
                                    , paragraph
                                        [ centerX
                                        , centerY
                                        ]
                                        [ el
                                            []
                                            (text m)
                                        ]
                                    ]
                                ]
                            ]
                        ]

                _ ->
                    none
    in
    row
        [ width fill
        , height fill
        , alignTop
        , inFront errView
        ]
        [ column
            [ width fill
            , height fill
            ]
            [ row
                [ width fill
                , height (px 40)
                , padding 5
                , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
                ]
                [ text ("Record URI: " ++ model.url)
                ]
            , row
                [ width fill
                , height fill
                ]
                [ html <|
                    HT.iframe
                        [ HA.src "./codeview.html"
                        , HA.style "border" "none"
                        , HA.style "margin" "0"
                        , HA.style "padding" "0"
                        , HA.style "height" "calc(100vh - 40px)"
                        , HA.style "width" "calc(100vw - 300px)"
                        ]
                        []
                ]
            ]
        , viewToolbar model
        ]


viewToolbar : Model -> Element Msg
viewToolbar model =
    column
        [ width (px 300)
        , height fill
        , alignTop
        , Background.color colourScheme.darkBlue
        , Border.widthEach { top = 0, bottom = 0, left = 1, right = 0 }
        ]
        [ row
            [ width fill
            , height fill
            , paddingXY 10 5
            , spacing 10
            , alignTop
            ]
            [ column
                [ width fill
                , alignTop
                ]
                [ row
                    [ width fill
                    , alignTop
                    ]
                    [ Input.radio
                        [ Font.size 14
                        , Font.color colourScheme.white
                        , spacing 10
                        , alignTop
                        , width fill
                        ]
                        { onChange = UserClickedApiFormatRadioButton
                        , selected = Just model.requestType
                        , label =
                            Input.labelAbove
                                [ Font.size 16
                                , Font.color colourScheme.white
                                , paddingXY 0 5
                                , Font.bold
                                , alignTop
                                ]
                                (text "API Format")
                        , options =
                            [ Input.option JsonLd (text "JSON-LD")
                            , Input.option Turtle (text "Turtle")
                            , Input.option NTriples (text "N-triples (RDF)")
                            ]
                        }
                    ]
                , viewLanguageRequestSelector model
                ]
            ]
        , formatCodeSnippet Python model
        ]


viewLanguageRequestSelector : Model -> Element Msg
viewLanguageRequestSelector model =
    let
        langSel =
            case model.languageRequest of
                AllLanguages ->
                    none

                SomeLanguages ->
                    viewLanguagesSelector model
    in
    row
        [ width fill
        , alignTop
        ]
        [ column
            [ width fill
            ]
            [ row
                [ width fill
                , alignTop
                ]
                [ Input.radio
                    [ Font.size 14
                    , Font.color colourScheme.white
                    , spacing 10
                    , alignTop
                    , width fill
                    ]
                    { onChange = UserClickedChooseLanguageRadioButton
                    , selected = Just model.languageRequest
                    , label =
                        Input.labelAbove
                            [ Font.size 16
                            , Font.color colourScheme.white
                            , paddingXY 0 5
                            , Font.bold
                            , alignTop
                            ]
                            (text "Response Languages")
                    , options =
                        [ Input.option AllLanguages (text "All languages")
                        , Input.option SomeLanguages (text "Selected languages")
                        ]
                    }
                ]
            , langSel
            ]
        ]


viewLanguagesSelector : Model -> Element Msg
viewLanguagesSelector model =
    let
        languageIsChecked lang =
            Maybe.withDefault [] model.chosenLanguages
                |> List.member lang
    in
    row
        [ width fill
        , alignTop
        , padding 10
        ]
        [ column
            [ width fill
            , alignTop
            , paddingXY 20 0
            , spacing 10
            ]
            [ Input.checkbox
                [ Font.size 14
                , Font.color colourScheme.white
                , alignTop
                ]
                { onChange = \state -> UserClickedSomeLanguageCheckboxSelector state English
                , icon = Input.defaultCheckbox
                , checked = languageIsChecked English
                , label = Input.labelRight [ Font.size 14 ] (text "English")
                }
            , Input.checkbox
                [ Font.size 14
                , Font.color colourScheme.white
                , alignTop
                ]
                { onChange = \state -> UserClickedSomeLanguageCheckboxSelector state German
                , icon = Input.defaultCheckbox
                , checked = languageIsChecked German
                , label = Input.labelRight [ Font.size 14 ] (text "German")
                }
            , Input.checkbox
                [ Font.size 14
                , Font.color colourScheme.white
                , alignTop
                ]
                { onChange = \state -> UserClickedSomeLanguageCheckboxSelector state French
                , icon = Input.defaultCheckbox
                , checked = languageIsChecked French
                , label = Input.labelRight [ Font.size 14 ] (text "French")
                }
            , Input.checkbox
                [ Font.size 14
                , Font.color colourScheme.white
                , alignTop
                ]
                { onChange = \state -> UserClickedSomeLanguageCheckboxSelector state Italian
                , icon = Input.defaultCheckbox
                , checked = languageIsChecked Italian
                , label = Input.labelRight [ Font.size 14 ] (text "Italian")
                }
            , Input.checkbox
                [ Font.size 14
                , Font.color colourScheme.white
                , alignTop
                ]
                { onChange = \state -> UserClickedSomeLanguageCheckboxSelector state Spanish
                , icon = Input.defaultCheckbox
                , checked = languageIsChecked Spanish
                , label = Input.labelRight [ Font.size 14 ] (text "Spanish")
                }
            , Input.checkbox
                [ Font.size 14
                , Font.color colourScheme.white
                , alignTop
                ]
                { onChange = \state -> UserClickedSomeLanguageCheckboxSelector state Portugese
                , icon = Input.defaultCheckbox
                , checked = languageIsChecked Portugese
                , label = Input.labelRight [ Font.size 14 ] (text "Portugese")
                }
            , Input.checkbox
                [ Font.size 14
                , Font.color colourScheme.white
                , alignTop
                ]
                { onChange = \state -> UserClickedSomeLanguageCheckboxSelector state Polish
                , icon = Input.defaultCheckbox
                , checked = languageIsChecked Polish
                , label = Input.labelRight [ Font.size 14 ] (text "Polish")
                }
            ]
        ]


formatCodeSnippet : CodeFormat -> Model -> Element Msg
formatCodeSnippet fmt model =
    let
        code =
            render
                { formattedAccept = formatAcceptHeader model.requestType
                , formattedLanguages = formatLangHeader model.chosenLanguages
                , uri = model.url
                }
                formatCurlCommand
    in
    row
        [ width fill
        , height (px 200)
        , Background.color colourScheme.white
        ]
        [ textColumn
            [ width fill
            , height fill
            , padding 4
            ]
            [ paragraph
                [ Font.family [ Font.monospace ]
                , Font.size 14
                , alignTop
                , htmlAttribute (HA.style "overflow-wrap" "anywhere")
                ]
                [ text code ]
            ]
        ]


type alias RequestComponents =
    { formattedAccept : String
    , formattedLanguages : Maybe String
    , uri : String
    }


formatCurlCommand : Template RequestComponents
formatCurlCommand =
    template "curl -XGET "
        |> withValue (\r -> " -H " ++ r.formattedAccept)
        |> withValue
            (\r ->
                Maybe.map (\hdr -> " -H " ++ hdr) r.formattedLanguages
                    |> Maybe.withDefault ""
            )
        |> withString " "
        |> withValue .uri


formatAcceptHeader : RequestType -> String
formatAcceptHeader reqType =
    "Accept: " ++ requestTypeToMimeType reqType


formatLangHeader : Maybe (List Language) -> Maybe String
formatLangHeader langList =
    Maybe.map
        (\ll ->
            List.map langToLangCode ll
                |> String.join ","
                |> String.append "X-API-Accept-Language: "
        )
        langList
