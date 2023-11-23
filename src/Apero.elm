module Apero exposing (..)

import Browser exposing (Document)
import Core exposing (Flags, Model, Msg(..), Response(..), getApiDocument, initBody)
import Element
import Ports exposing (OutgoingMessage(..), encodeMessageForPortSend, sendOutgoingMessageOnPort)
import Ui.View


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        body =
            initBody flags
    in
    ( body
    , getApiDocument body.url body.requestType
    )


view : Model -> Document Msg
view model =
    { title = "Apero"
    , body =
        [ Element.layout [] (Ui.View.view model)
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ServerRespondedWithApiDocument (Ok ( _, response )) ->
            ( { model
                | serverResponse = Response response
              }
            , LoadApiResponse response
                |> encodeMessageForPortSend
                |> sendOutgoingMessageOnPort
            )

        ServerRespondedWithApiDocument (Err error) ->
            ( { model
                | serverResponse = Error error
              }
            , Cmd.none
            )

        NothingHappened ->
            ( model, Cmd.none )


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
