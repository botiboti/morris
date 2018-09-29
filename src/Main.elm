module Main exposing (..)

import Browser exposing (element)
import Html exposing (..)
import Morris exposing (..)
import View exposing (..)


main =
    Browser.element
        { init = init
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        , view = View.view
        }
