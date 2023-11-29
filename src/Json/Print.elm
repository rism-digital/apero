module Json.Print exposing (Config, prettyString, prettyValue)

{-| Pretty print JSON stored as a `String` or `Json.Encode.Value`

@docs Config, prettyString, prettyValue

From <https://package.elm-lang.org/packages/the-sett/elm-pretty-printer/latest/Pretty>, updated to newer version
of the Pretty library.

-}

-- third

import Json.Decode as Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Pretty exposing (Doc, append, char, join, line, nest, space, string, surround)



-- UTIL


commaLine : Doc t
commaLine =
    append (char ',') line


colonSpace : Doc t
colonSpace =
    append (char ':') space


openBrace : Doc t
openBrace =
    append (char '{') line


closeBrace : Doc t
closeBrace =
    append line (char '}')


openBracket : Doc t
openBracket =
    append (char '[') line


closeBracket : Doc t
closeBracket =
    append line (char ']')



-- CONVERT


nullToDoc : Maybe (Doc t) -> Doc t
nullToDoc maybeDoc =
    case maybeDoc of
        Just doc ->
            doc

        Nothing ->
            string "null"


stringToDoc : String -> Doc t
stringToDoc s =
    surround (char '"') (char '"') (string s)


numberToDoc : Float -> Doc t
numberToDoc num =
    string (String.fromFloat num)


boolToDoc : Bool -> Doc t
boolToDoc bool =
    if bool then
        string "true"

    else
        string "false"


objectToDoc : Int -> List ( String, Doc t ) -> Doc t
objectToDoc indent pairs =
    if List.isEmpty pairs then
        string "{}"

    else
        append
            (nest
                indent
                (append
                    openBrace
                    (join
                        (append (char ',') line)
                        (List.map
                            (\( key, doc ) ->
                                append (stringToDoc key) (append colonSpace doc)
                            )
                            pairs
                        )
                    )
                )
            )
            closeBrace


listToDoc : Int -> List (Doc t) -> Doc t
listToDoc indent list =
    if List.isEmpty list then
        string "[]"

    else
        append
            (nest
                indent
                (append
                    openBracket
                    (join commaLine list)
                )
            )
            closeBracket



-- DECODE


decodeDoc : Int -> Decoder (Doc t)
decodeDoc indent =
    Decode.map
        nullToDoc
        (Decode.maybe
            (Decode.oneOf
                [ Decode.map stringToDoc Decode.string
                , Decode.map numberToDoc Decode.float
                , Decode.map boolToDoc Decode.bool
                , Decode.map (listToDoc indent) (Decode.lazy (\_ -> Decode.list (decodeDoc indent)))
                , Decode.map (objectToDoc indent) (Decode.lazy (\_ -> Decode.keyValuePairs (decodeDoc indent)))
                ]
            )
        )



-- PRETTY


{-| Formatting configuration.

`indent` is the number of spaces in an indent.

`columns` is the desired column width of the formatted string. The formatter
will try to fit it as best as possible to the column width, but can still
exceed this limit. The maximum column width of the formatted string is
unbounded.

-}
type alias Config =
    { indent : Int
    , columns : Int
    }


{-| Formats a JSON string.
passes the string through `Json.Decode.decodeString` and bubbles up any JSON
parsing errors.
-}
prettyString : Config -> String -> Result String String
prettyString { columns, indent } json =
    Decode.decodeString (decodeDoc indent) json
        |> Result.map (Pretty.pretty columns)
        |> Result.mapError Decode.errorToString


{-| Formats a `Json.Encode.Value`. Internally passes the string through
`Json.Decode.decodeValue` and bubbles up any JSON parsing errors.
-}
prettyValue : Config -> Value -> Result String String
prettyValue { columns, indent } json =
    Decode.decodeValue (decodeDoc indent) json
        |> Result.map (Pretty.pretty columns)
        |> Result.mapError Decode.errorToString
