module Core exposing (..)

import Http exposing (emptyBody)
import Http.Detailed


type RequestType
    = JsonLd
    | Turtle
    | NTriples


type Language
    = English
    | German
    | French
    | Italian
    | Spanish
    | Portugese
    | Polish


type LanguageSelection
    = AllLanguages
    | SomeLanguages


type Response data
    = Loading (Maybe data)
    | Response data
    | Error (Http.Detailed.Error String)
    | NoResponseToShow


type CodeFormat
    = CURL
    | Python
    | JavaScript


type alias Flags =
    { url : String
    , requestType : String
    }


type alias Model =
    { url : String
    , requestType : RequestType
    , serverResponse : Response String
    , languageRequest : LanguageSelection
    , chosenLanguages : Maybe (List Language)
    }


type alias ErrorBody =
    { errorMessage : String }


type Msg
    = ServerRespondedWithApiDocument (Result (Http.Detailed.Error String) ( Http.Metadata, String ))
    | UserClickedApiFormatRadioButton RequestType
    | UserClickedChooseLanguageRadioButton LanguageSelection
    | UserClickedSomeLanguageCheckboxSelector Bool Language
    | UserClickedErrorMessageDismiss
    | NothingHappened


convertResponseType : String -> RequestType
convertResponseType rtype =
    case rtype of
        "json-ld" ->
            JsonLd

        "turtle" ->
            Turtle

        "n-triples" ->
            NTriples

        _ ->
            JsonLd


requestTypeToMimeType : RequestType -> String
requestTypeToMimeType requestType =
    case requestType of
        JsonLd ->
            "application/ld+json"

        Turtle ->
            "text/turtle"

        NTriples ->
            "application/n-triples"


initBody : Flags -> Model
initBody flags =
    { url = flags.url
    , requestType = convertResponseType flags.requestType
    , serverResponse = Loading Nothing
    , languageRequest = AllLanguages
    , chosenLanguages = Nothing
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
        Nothing ->
            headerList

        Just requestedLangs ->
            let
                langHeader =
                    List.map langToLangCode requestedLangs
                        |> String.join ","
                        |> Http.header "X-API-Accept-Language"
            in
            langHeader :: headerList


langToLangCode : Language -> String
langToLangCode language =
    case language of
        English ->
            "en"

        German ->
            "de"

        Polish ->
            "pl"

        Portugese ->
            "pt"

        French ->
            "fr"

        Italian ->
            "it"

        Spanish ->
            "es"
