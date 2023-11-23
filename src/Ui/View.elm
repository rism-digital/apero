module Ui.View exposing (..)

import Core exposing (Model, Msg)
import Element exposing (Element, column, fill, height, none, row, width)


view : Model -> Element Msg
view model =
    row
        [ width fill
        , height fill
        ]
        [ column
            []
            []
        ]
