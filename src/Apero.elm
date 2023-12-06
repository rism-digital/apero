module Apero exposing (..)

import Browser exposing (Document)
import Core exposing (Flags, Language(..), LanguageSelection(..), Model, Msg(..), Response(..), getApiDocument, initBody)
import Element exposing (fill, height, padding, width)
import Ui.View


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        body =
            initBody flags
    in
    ( body
    , getApiDocument
        { requestType = body.requestType
        , requestLanguages = body.chosenLanguages
        }
        body.url
    )


view : Model -> Document Msg
view model =
    { title = "Apero"
    , body =
        [ Ui.View.view model
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ServerRespondedWithApiDocument (Ok ( _, response )) ->
            ( { model
                | serverResponse = Response response
              }
            , Cmd.none
            )

        ServerRespondedWithApiDocument (Err err) ->
            ( { model
                | serverResponse = Error err
              }
            , Cmd.none
            )

        UserClickedErrorMessageDismiss ->
            ( { model
                | serverResponse = NoResponseToShow
              }
            , Cmd.none
            )

        UserClickedApiFormatRadioButton format ->
            ( { model
                | requestType = format
              }
            , getApiDocument
                { requestType = format
                , requestLanguages = model.chosenLanguages
                }
                model.url
            )

        UserClickedChooseLanguageRadioButton selection ->
            let
                languageList =
                    case selection of
                        AllLanguages ->
                            Nothing

                        SomeLanguages ->
                            Just [ English ]
            in
            ( { model
                | languageRequest = selection
                , chosenLanguages = languageList
              }
            , getApiDocument
                { requestType = model.requestType
                , requestLanguages = languageList
                }
                model.url
            )

        UserClickedSomeLanguageCheckboxSelector checked language ->
            let
                newLangList =
                    Maybe.map
                        (\languages ->
                            if checked == False then
                                List.filter ((/=) language) languages

                            else
                                language :: languages
                        )
                        model.chosenLanguages
            in
            ( { model
                | chosenLanguages = newLangList
              }
            , getApiDocument
                { requestType = model.requestType
                , requestLanguages = newLangList
                }
                model.url
            )

        NothingHappened ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
