module View exposing (..)

import Animation exposing (render, style)
import Browser.Events exposing (onAnimationFrame)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List.Extra
import Morris exposing (..)
import Svg exposing (text_)
import Svg.Attributes exposing (fill, string)
import Tests exposing (..)
import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (..)
import TypedSvg.Color exposing (..)
import TypedSvg.Core exposing (..)
import TypedSvg.Events exposing (..)
import TypedSvg.Types exposing (..)


view : Model -> Html Morris.Msg
view model =
    div
        [ Html.Attributes.style "font-family" "monospace"
        , Html.Attributes.style "font-size" "42px"
        ]
        [ div [] [ viewBoard model ]
        , button [ Html.Events.onClick Reset ] [ Html.text "New Game" ]
        , viewTests
        , svg
            [ TypedSvg.Attributes.height (px 1000)
            , TypedSvg.Attributes.width (px 1000)
            ]
            [ Svg.text_
                [ x (px 0)
                , y (px 0)
                , Svg.Attributes.string "Hello"
                ]
                []
            ]
        ]


viewBoard : Model -> Html Morris.Msg
viewBoard model =
    let
        -- Returns the player on the location.
        get n =
            model.board |> List.Extra.getAt (locationIndex n) |> Maybe.withDefault Empty

        -- Determine if the location is empty.
        isOccupied location =
            get location == Empty

        -- Determine if the location is the desired one to move.
        isSelected location =
            case model.moveInProgress of
                FirstClick loc ->
                    if (location == loc) && (model.counter >= 18) then
                        True

                    else
                        False

                _ ->
                    False

        gT18Counter =
            model.counter > 18

        lT3PiecesFor player =
            List.length (playerLocations player model.board) < 3

        noValidMovesFor player =
            not
                (playerLocations player model.board
                    |> List.any
                        (\playerLoc ->
                            playerLocations Empty model.board
                                |> List.any
                                    (\emptyLoc -> allowedMove playerLoc emptyLoc)
                        )
                )

        isLoserPiece location =
            gT18Counter
                && (lT3PiecesFor (get location) || noValidMovesFor (get location))

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
                            , if isOccupied xyz then
                                r (px 10)

                              else
                                r (px 17)
                            , if isSelected xyz then
                                TypedSvg.Attributes.strokeWidth (px 5)

                              else
                                TypedSvg.Attributes.strokeWidth (px 0)
                            , TypedSvg.Color.rgb 255 255 77
                                |> TypedSvg.Attributes.stroke
                            , TypedSvg.Attributes.fill <| playerColor (get xyz)
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
            , stroke <| TypedSvg.Color.rgb 51 0 0
            ]
            []
         , rect
            [ x (px coordinates.c2)
            , y (px coordinates.c2)
            , TypedSvg.Attributes.width (px 240)
            , TypedSvg.Attributes.height (px 240)
            , strokeWidth (px 5)
            , noFill
            , stroke <| TypedSvg.Color.rgb 51 0 0
            ]
            []
         , rect
            [ x (px coordinates.c3)
            , y (px coordinates.c3)
            , TypedSvg.Attributes.width (px 120)
            , TypedSvg.Attributes.height (px 120)
            , strokeWidth (px 5)
            , noFill
            , stroke <| TypedSvg.Color.rgb 51 0 0
            ]
            []
         , line
            [ x1 (px coordinates.c4)
            , y1 (px coordinates.c1)
            , x2 (px coordinates.c4)
            , y2 (px coordinates.c3)
            , strokeWidth (px 5)
            , stroke <| TypedSvg.Color.rgb 51 0 0
            ]
            []
         , line
            [ x1 (px coordinates.c1)
            , y1 (px coordinates.c4)
            , x2 (px coordinates.c3)
            , y2 (px coordinates.c4)
            , strokeWidth (px 5)
            , stroke <| TypedSvg.Color.rgb 51 0 0
            ]
            []
         , line
            [ x1 (px coordinates.c5)
            , y1 (px coordinates.c4)
            , x2 (px coordinates.c7)
            , y2 (px coordinates.c4)
            , strokeWidth (px 5)
            , stroke <| TypedSvg.Color.rgb 51 0 0
            ]
            []
         , line
            [ x1 (px coordinates.c4)
            , y1 (px coordinates.c5)
            , x2 (px coordinates.c4)
            , y2 (px coordinates.c7)
            , strokeWidth (px 5)
            , stroke <| TypedSvg.Color.rgb 51 0 0
            ]
            []
         , circle
            (Animation.render model.style
                ++ [ cx (px 510)
                   , r (px 14)
                   , TypedSvg.Attributes.fill <| Fill purple
                   ]
            )
            []
         ]
            ++ (allLocations |> List.map circ)
            ++ [ rect
                    [ x (px 480)
                    , y (px 80)
                    , rx (px 30)
                    , ry (px 32)
                    , TypedSvg.Attributes.width (px 60)
                    , TypedSvg.Attributes.height (px 240)
                    , Svg.Attributes.fill <| "none "
                    , TypedSvg.Attributes.strokeWidth (px 3)
                    , TypedSvg.Attributes.stroke <| black
                    ]
                    []
               , circle
                    [ cx (px 580)
                    , cy (px 110)
                    , r (px 17)
                    , TypedSvg.Color.rgb 222 184 135
                        |> Fill
                        |> TypedSvg.Attributes.fill
                    ]
                    []
               , circle
                    [ cx (px 580)
                    , cy (px 290)
                    , r (px 17)
                    ]
                    []
               ]
        )


playerColor : Player -> Fill
playerColor player =
    case player of
        Empty ->
            Fill <| TypedSvg.Color.rgb 51 0 0

        W ->
            TypedSvg.Color.rgb 222 184 135
                |> Fill

        B ->
            TypedSvg.Color.rgb 0 0 0
                |> Fill


viewTests : Html Morris.Msg
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
