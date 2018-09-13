module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List.Extra
import Morris exposing (..)
import Tests exposing (..)
import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (..)
import TypedSvg.Color exposing (..)
import TypedSvg.Core exposing (..)
import TypedSvg.Events exposing (..)
import TypedSvg.Types exposing (..)


view : Model -> Html Msg
view model =
    div
        [ Html.Attributes.style "font-family" "monospace"
        , Html.Attributes.style "font-size" "42px"
        ]
        [ div [] [ viewBoard model ]
        , button [ Html.Events.onClick Reset ] [ Html.text "New Game" ]
        , div []
            [ Html.text
                (if isWin model then
                    "WIN"

                 else
                    " "
                )
            ]
        , viewTests
        ]


viewBoard : Model -> Html Msg
viewBoard model =
    let
        get n =
            model.board |> List.Extra.getAt (locationIndex n) |> Maybe.withDefault Empty

        locationColor location =
            case model.moveInProgress of
                FirstClick loc ->
                    if (location == loc) && (model.counter >= 18) then
                        Fill yellow

                    else
                        playerColor (get location)

                _ ->
                    playerColor (get location)

        coordinates =
            { c1 = 20, c2 = 80, c3 = 140, c4 = 200, c5 = 260, c6 = 320, c7 = 380 }

        ring x y a b c =
            case ( x, y ) of
                ( X1, Y1 ) ->
                    ( a, a )

                ( X1, Y2 ) ->
                    ( a, b )

                ( X1, Y3 ) ->
                    ( a, c )

                ( X2, Y1 ) ->
                    ( b, a )

                ( X2, Y2 ) ->
                    ( b, b )

                ( X2, Y3 ) ->
                    ( b, c )

                ( X3, Y1 ) ->
                    ( c, a )

                ( X3, Y2 ) ->
                    ( c, b )

                ( X3, Y3 ) ->
                    ( c, c )

        coord ( x, y, z ) =
            case z of
                Z1 ->
                    ring x y coordinates.c1 coordinates.c4 coordinates.c7

                Z2 ->
                    ring x y coordinates.c2 coordinates.c4 coordinates.c6

                Z3 ->
                    ring x y coordinates.c3 coordinates.c4 coordinates.c5

        circ xyz =
            coord xyz
                |> (\( x, y ) ->
                        circle
                            [ cx (px x)
                            , cy (px y)
                            , r (px 12)
                            , fill <| locationColor xyz
                            , TypedSvg.Events.onClick (Click xyz)
                            ]
                            []
                   )
    in
    svg
        [ viewBox 0 0 1000 400
        ]
        ([ rect
            [ x (px coordinates.c1)
            , y (px coordinates.c1)
            , TypedSvg.Attributes.width (px 360)
            , TypedSvg.Attributes.height (px 360)
            , strokeWidth (px 5)
            , noFill
            , stroke black
            ]
            []
         , rect
            [ x (px coordinates.c2)
            , y (px coordinates.c2)
            , TypedSvg.Attributes.width (px 240)
            , TypedSvg.Attributes.height (px 240)
            , strokeWidth (px 5)
            , noFill
            , stroke black
            ]
            []
         , rect
            [ x (px coordinates.c3)
            , y (px coordinates.c3)
            , TypedSvg.Attributes.width (px 120)
            , TypedSvg.Attributes.height (px 120)
            , strokeWidth (px 5)
            , noFill
            , stroke black
            ]
            []
         , line
            [ x1 (px coordinates.c4)
            , y1 (px coordinates.c1)
            , x2 (px coordinates.c4)
            , y2 (px coordinates.c3)
            , strokeWidth (px 5)
            , stroke black
            ]
            []
         , line
            [ x1 (px coordinates.c1)
            , y1 (px coordinates.c4)
            , x2 (px coordinates.c3)
            , y2 (px coordinates.c4)
            , strokeWidth (px 5)
            , stroke black
            ]
            []
         , line
            [ x1 (px coordinates.c5)
            , y1 (px coordinates.c4)
            , x2 (px coordinates.c7)
            , y2 (px coordinates.c4)
            , strokeWidth (px 5)
            , stroke black
            ]
            []
         , line
            [ x1 (px coordinates.c4)
            , y1 (px coordinates.c5)
            , x2 (px coordinates.c4)
            , y2 (px coordinates.c7)
            , strokeWidth (px 5)
            , stroke black
            ]
            []
         , TypedSvg.text_
            [ cx (px 200)
            , cy (px 200)
            ]
            [ TypedSvg.Core.text "HH" ]
         ]
            ++ (allLocations |> List.map circ)
        )


playerColor : Player -> Fill
playerColor player =
    case player of
        Empty ->
            Fill black

        W ->
            Fill red

        B ->
            Fill green


viewTests : Html Msg
viewTests =
    let
        pass =
            List.all identity tests
    in
    div []
        [ h3
            [ Html.Attributes.style "color"
                (if pass then
                    "darkgreen"

                 else
                    "darkred"
                )
            ]
            [ Html.text ("Tests: " ++ toCheck pass)
            ]
        , div [] [ Html.text <| String.join "" (List.map toCheck tests) ]
        ]


toCheck : Bool -> String
toCheck bool =
    case bool of
        True ->
            "✔"

        False ->
            "✘"
