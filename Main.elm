module Main exposing (..)

import Html exposing (..)
import Morris exposing (..)
import View exposing (..)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    program
        { init = init
        , view = View.view
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        }
