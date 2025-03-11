module Ui.View exposing (RequestComponents, view)

import Core exposing (BodyView(..), CodeFormat(..), Language(..), LanguageSelection(..), Model, Msg(..), RequestType(..), Response(..), langToLangCode, requestTypeToMimeType)
import Element exposing (Color, Element, alignRight, alignTop, centerX, centerY, clipY, column, el, fill, fromRgb255, height, html, htmlAttribute, inFront, none, padding, paddingXY, paragraph, pointer, px, rgb255, row, scrollbars, spacing, text, textColumn, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html as HT
import Html.Attributes as HA
import Http.Detailed
import Json.Print
import SyntaxHighlight exposing (gitHub, json, noLang, toBlockHtml, useTheme, xml)
import Template exposing (Template, render, template, withString, withValue)


colourScheme :
    { darkBlue : Color
    , translucentGrey : Color
    , white : Color
    }
colourScheme =
    { darkBlue = rgb255 29 53 87
    , translucentGrey =
        fromRgb255
            { alpha = 0.5
            , blue = 153
            , green = 136
            , red = 119
            }
    , white = rgb255 255 255 255
    }


view : Model -> HT.Html Msg
view model =
    case model.view of
        Viewer ->
            viewer model

        Raw ->
            raw model


raw : Model -> HT.Html Msg
raw model =
    case model.serverResponse of
        Response docString ->
            HT.text docString

        _ ->
            HT.text ""


viewer : Model -> HT.Html Msg
viewer model =
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
    Element.layout
        [ width fill
        , height fill
        , padding 0
        ]
        (row
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
                    , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                    ]
                    [ text ("Record URI: " ++ model.url)
                    ]
                , viewCode model
                ]
            , viewToolbar model
            ]
        )


viewCode : Model -> Element Msg
viewCode model =
    let
        fmtOutput =
            case model.serverResponse of
                Response docString ->
                    case model.requestType of
                        JsonLd ->
                            Json.Print.prettyString { indent = 2, columns = 80 } docString
                                |> Result.withDefault docString
                                |> json
                                |> Result.map (toBlockHtml (Just 1))
                                |> Result.withDefault (HT.text docString)

                        MarcXML ->
                            xml docString
                                |> Result.map (toBlockHtml (Just 1))
                                |> Result.withDefault (HT.text docString)

                        _ ->
                            noLang docString
                                |> Result.map (toBlockHtml (Just 1))
                                |> Result.withDefault (HT.text docString)

                _ ->
                    HT.text "Loading ..."
    in
    row
        [ width fill
        , height fill
        , clipY
        , htmlAttribute (HA.style "min-height" "unset")
        ]
        [ column
            [ width fill
            , height fill
            , Font.size 14
            , scrollbars
            , paddingXY 10 0
            ]
            [ html
                (HT.div
                    [ HA.style "overflow-wrap" "anywhere"
                    , HA.style "width" "calc(100vw - 300px)"
                    , HA.style "line-height" "1.4em"
                    ]
                    [ useTheme gitHub
                    , fmtOutput
                    ]
                )
            ]
        ]


viewToolbar : Model -> Element Msg
viewToolbar model =
    column
        [ width (px 300)
        , height fill
        , alignTop
        , Background.color colourScheme.darkBlue
        , Border.widthEach { bottom = 0, left = 1, right = 0, top = 0 }
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
                        { label =
                            Input.labelAbove
                                [ Font.size 16
                                , Font.color colourScheme.white
                                , paddingXY 0 5
                                , Font.bold
                                , alignTop
                                ]
                                (text "API Format")
                        , onChange = UserClickedApiFormatRadioButton
                        , options =
                            [ Input.option JsonLd (text "JSON-LD")
                            , Input.option Turtle (text "Turtle")
                            , Input.option NTriples (text "N-triples")
                            , Input.option MarcXML (text "MARCXML")
                            ]
                        , selected = Just model.requestType
                        }
                    ]
                , case model.requestType of
                    MarcXML ->
                        none

                    _ ->
                        viewLanguageRequestSelector model
                ]
            ]
        , formatCodeSnippet CURL model
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
                    { label =
                        Input.labelAbove
                            [ Font.size 16
                            , Font.color colourScheme.white
                            , paddingXY 0 5
                            , Font.bold
                            , alignTop
                            ]
                            (text "Response Languages")
                    , onChange = UserClickedChooseLanguageRadioButton
                    , options =
                        [ Input.option AllLanguages (text "All languages")
                        , Input.option SomeLanguages (text "Selected languages")
                        ]
                    , selected = Just model.languageRequest
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
                { checked = languageIsChecked English
                , icon = Input.defaultCheckbox
                , label = Input.labelRight [ Font.size 14 ] (text "English")
                , onChange = \state -> UserClickedSomeLanguageCheckboxSelector state English
                }
            , Input.checkbox
                [ Font.size 14
                , Font.color colourScheme.white
                , alignTop
                ]
                { checked = languageIsChecked German
                , icon = Input.defaultCheckbox
                , label = Input.labelRight [ Font.size 14 ] (text "German")
                , onChange = \state -> UserClickedSomeLanguageCheckboxSelector state German
                }
            , Input.checkbox
                [ Font.size 14
                , Font.color colourScheme.white
                , alignTop
                ]
                { checked = languageIsChecked French
                , icon = Input.defaultCheckbox
                , label = Input.labelRight [ Font.size 14 ] (text "French")
                , onChange = \state -> UserClickedSomeLanguageCheckboxSelector state French
                }
            , Input.checkbox
                [ Font.size 14
                , Font.color colourScheme.white
                , alignTop
                ]
                { checked = languageIsChecked Italian
                , icon = Input.defaultCheckbox
                , label = Input.labelRight [ Font.size 14 ] (text "Italian")
                , onChange = \state -> UserClickedSomeLanguageCheckboxSelector state Italian
                }
            , Input.checkbox
                [ Font.size 14
                , Font.color colourScheme.white
                , alignTop
                ]
                { checked = languageIsChecked Spanish
                , icon = Input.defaultCheckbox
                , label = Input.labelRight [ Font.size 14 ] (text "Spanish")
                , onChange = \state -> UserClickedSomeLanguageCheckboxSelector state Spanish
                }
            , Input.checkbox
                [ Font.size 14
                , Font.color colourScheme.white
                , alignTop
                ]
                { checked = languageIsChecked Portuguese
                , icon = Input.defaultCheckbox
                , label = Input.labelRight [ Font.size 14 ] (text "Portuguese")
                , onChange = \state -> UserClickedSomeLanguageCheckboxSelector state Portuguese
                }
            , Input.checkbox
                [ Font.size 14
                , Font.color colourScheme.white
                , alignTop
                ]
                { checked = languageIsChecked Polish
                , icon = Input.defaultCheckbox
                , label = Input.labelRight [ Font.size 14 ] (text "Polish")
                , onChange = \state -> UserClickedSomeLanguageCheckboxSelector state Polish
                }
            ]
        ]


formatCodeSnippet : CodeFormat -> Model -> Element Msg
formatCodeSnippet _ model =
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
        ]
        [ textColumn
            [ width fill
            , height fill
            , spacing 4
            ]
            [ row
                [ width fill
                , Font.size 14
                , Font.semiBold
                , Font.color colourScheme.white
                , padding 4
                ]
                [ text "API Request"
                ]
            , paragraph
                [ Background.color colourScheme.white
                , Font.family [ Font.monospace ]
                , Font.size 14
                , alignTop
                , htmlAttribute (HA.style "overflow-wrap" "anywhere")
                , padding 4
                , height fill
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
        |> withValue (\r -> " -H " ++ "\"" ++ r.formattedAccept ++ "\"")
        |> withValue
            (\r ->
                Maybe.map (\hdr -> " -H " ++ "\"" ++ hdr ++ "\"") r.formattedLanguages
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
