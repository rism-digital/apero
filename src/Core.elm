module Core exposing (..)

import Http exposing (emptyBody)
import Http.Detailed


type RequestType
    = JsonLd
    | Turtle
    | NQuads


type Response data
    = Loading (Maybe data)
    | Response data
    | Error (Http.Detailed.Error String)
    | NoResponseToShow


type alias Flags =
    { url : String
    , requestType : String
    }


type alias Model =
    { url : String
    , requestType : RequestType
    , serverResponse : Response String
    }


type alias ErrorBody =
    { errorMessage : String }


type Msg
    = ServerRespondedWithApiDocument (Result (Http.Detailed.Error String) ( Http.Metadata, String ))
    | NothingHappened


convertResponseType : String -> RequestType
convertResponseType rtype =
    case rtype of
        "json-ld" ->
            JsonLd

        "turtle" ->
            Turtle

        "n-quads" ->
            NQuads

        _ ->
            JsonLd


requestTypeToMimeType : RequestType -> String
requestTypeToMimeType requestType =
    case requestType of
        JsonLd ->
            "application/ld+json"

        Turtle ->
            "text/turtle"

        NQuads ->
            "application/n-quads"


initBody : Flags -> Model
initBody flags =
    { url = flags.url
    , requestType = convertResponseType flags.requestType
    , serverResponse = Loading Nothing
    }


getApiDocument : String -> RequestType -> Cmd Msg
getApiDocument url requestType =
    Http.request
        { method = "GET"
        , headers =
            [ requestTypeToMimeType requestType
                |> Http.header "Accept"
            ]
        , url = url
        , body = emptyBody
        , expect = Http.Detailed.expectString ServerRespondedWithApiDocument
        , timeout = Nothing
        , tracker = Nothing
        }
