port module Ports exposing (..)

import Json.Decode as D
import Json.Encode as E


type OutgoingMessage
    = LoadApiResponse String


port sendOutgoingMessageOnPort : E.Value -> Cmd msg


port receiveIncomingMessageFromPort : (D.Value -> msg) -> Sub msg


encodeMessageForPortSend : OutgoingMessage -> E.Value
encodeMessageForPortSend msg =
    convertOutgoingMessageToJsonMessage msg
        |> E.object


convertOutgoingMessageToJsonMessage : OutgoingMessage -> List ( String, E.Value )
convertOutgoingMessageToJsonMessage msg =
    case msg of
        LoadApiResponse body ->
            [ ( "message", E.string "load-code" )
            , ( "params"
              , E.object
                    [ ( "body", E.string body ) ]
              )
            ]
