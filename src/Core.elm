module Core exposing (BodyView(..), CodeFormat(..), Flags, Language(..), LanguageSelection(..), Model, Msg(..), RequestConfig, RequestType(..), Response(..), getApiDocument, initBody, langToLangCode, requestTypeToMimeType)

import Http exposing (emptyBody)
import Http.Detailed


type BodyView
    = Viewer
    | Raw


type RequestType
    = JsonLd
    | Turtle
    | NTriples
    | MarcXML


type Language
    = English
    | German
    | French
    | Italian
    | Spanish
    | Portuguese
    | Polish


type LanguageSelection
    = AllLanguages
    | SomeLanguages


type Response data
    = Loading
    | Response data
    | Error (Http.Detailed.Error String)
    | NoResponseToShow


type CodeFormat
    = CURL


type alias Flags =
    { url : String
    , requestType : String
    , view : String
    }


type alias Model =
    { url : String
    , requestType : RequestType
    , serverResponse : Response String
    , languageRequest : LanguageSelection
    , chosenLanguages : Maybe (List Language)
    , view : BodyView
    }


type Msg
    = ServerRespondedWithApiDocument (Result (Http.Detailed.Error String) ( Http.Metadata, String ))
    | UserClickedApiFormatRadioButton RequestType
    | UserClickedChooseLanguageRadioButton LanguageSelection
    | UserClickedSomeLanguageCheckboxSelector Bool Language
    | UserClickedErrorMessageDismiss


convertResponseType : String -> RequestType
convertResponseType rtype =
    case rtype of
        "json-ld" ->
            JsonLd

        "marcxml" ->
            MarcXML

        "n-triples" ->
            NTriples

        "turtle" ->
            Turtle

        _ ->
            JsonLd


convertViewType : String -> BodyView
convertViewType view =
    if view == "raw" then
        Raw

    else
        Viewer


requestTypeToMimeType : RequestType -> String
requestTypeToMimeType requestType =
    case requestType of
        JsonLd ->
            "application/ld+json"

        Turtle ->
            "text/turtle"

        NTriples ->
            "application/n-triples"

        MarcXML ->
            "application/marcxml+xml"


initBody : Flags -> Model
initBody flags =
    { url = flags.url
    , requestType = convertResponseType flags.requestType
    , serverResponse = Loading
    , languageRequest = AllLanguages
    , chosenLanguages = Nothing
    , view = convertViewType flags.view
    }


type alias RequestConfig =
    { requestType : RequestType
    , requestLanguages : Maybe (List Language)
    }


getApiDocument : RequestConfig -> String -> Cmd Msg
getApiDocument cfg url =
    let
        headers =
            [ requestTypeToMimeType cfg.requestType
                |> Http.header "Accept"
            ]
                |> addLangToHeaders cfg.requestLanguages
    in
    Http.request
        { method = "GET"
        , headers = headers
        , url = url
        , body = emptyBody
        , expect = Http.Detailed.expectString ServerRespondedWithApiDocument
        , timeout = Nothing
        , tracker = Nothing
        }


addLangToHeaders : Maybe (List Language) -> List Http.Header -> List Http.Header
addLangToHeaders langList headerList =
    case langList of
        Just requestedLangs ->
            let
                langHeader =
                    List.map langToLangCode requestedLangs
                        |> String.join ","
                        |> Http.header "X-API-Accept-Language"
            in
            langHeader :: headerList

        Nothing ->
            headerList


langToLangCode : Language -> String
langToLangCode language =
    case language of
        English ->
            "en"

        German ->
            "de"

        French ->
            "fr"

        Italian ->
            "it"

        Spanish ->
            "es"

        Portuguese ->
            "pt"

        Polish ->
            "pl"
