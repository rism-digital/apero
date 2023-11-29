port module Ports exposing (..)

import Core exposing (Model, Msg(..))
import Dict
import Http.Detailed
import Json.Decode as D
import Json.Decode.Pipeline exposing (required)
import Json.Encode as E
import Json.Print


type AperoMessage
    = CodeMirrorIsReady
    | UnknownMessage


type IncomingMessage
    = IncomingMessage AperoMessage D.Value


type OutgoingMessage
    = LoadDocument String String


port sendOutgoingMessageOnPort : E.Value -> Cmd msg


port receiveIncomingMessageFromPort : (D.Value -> msg) -> Sub msg


encodeMessageForPortSend : OutgoingMessage -> E.Value
encodeMessageForPortSend msg =
    convertOutgoingMessageToJsonMessage msg
        |> E.object


convertOutgoingMessageToJsonMessage : OutgoingMessage -> List ( String, E.Value )
convertOutgoingMessageToJsonMessage msg =
    case msg of
        LoadDocument docString documentSettings ->
            let
                fmtJson =
                    Json.Print.prettyString { indent = 4, columns = 120 } docString
                        |> Result.withDefault docString
            in
            [ ( "message", E.string "load-document" )
            , ( "params"
              , E.object
                    [ ( "document", E.string fmtJson )
                    , ( "format", E.string documentSettings )
                    ]
              )
            ]


aperoStringMessageMap : List ( String, AperoMessage )
aperoStringMessageMap =
    [ ( "codemirror-ready", CodeMirrorIsReady ) ]


convertFunctionStringToVerovioMessage : String -> AperoMessage
convertFunctionStringToVerovioMessage methodStr =
    Dict.fromList aperoStringMessageMap
        |> Dict.get methodStr
        |> Maybe.withDefault UnknownMessage


decodeMessageType : D.Decoder AperoMessage
decodeMessageType =
    D.map convertFunctionStringToVerovioMessage D.string


incomingMessageDecoder : D.Decoder IncomingMessage
incomingMessageDecoder =
    D.succeed IncomingMessage
        |> required "message" decodeMessageType
        |> required "result" D.value


incomingMessagesHelper : Model -> E.Value -> Msg
incomingMessagesHelper _ value =
    case D.decodeValue incomingMessageDecoder value of
        Ok inMessage ->
            let
                (IncomingMessage msg result) =
                    inMessage
            in
            case msg of
                CodeMirrorIsReady ->
                    CodeMirrorSentReadySignal

                UnknownMessage ->
                    NothingHappened

        Err _ ->
            NothingHappened
