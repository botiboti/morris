module View exposing (..)

import Color exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List.Extra
import Morris exposing (..)
import Tests exposing (..)
import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (..)
import TypedSvg.Events exposing (..)
import TypedSvg.Types exposing (..)


view : Model -> Html Msg
view model =
    div [ Html.Attributes.style [ ( "font-family", "monospace" ), ( "font-size", "42px" ) ] ]
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
            model.board |> List.Extra.getAt n |> Maybe.withDefault Empty

        locationcolor location model =
            if
                (case model.moveinprogress of
                    FirstClick loc ->
                        loc

                    _ ->
                        ( X2, Y2, Z1 )
                )
                    == location
                    && model.counter
                    >= 18
            then
                Fill yellow

            else
                playerColor (get (locationIndex location))

        ( c1, c2, c3, c4, c5, c6, c7 ) =
            ( 20, 80, 140, 200, 260, 320, 380 )

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
                    ring x y c1 c4 c7

                Z2 ->
                    ring x y c2 c4 c6

                Z3 ->
                    ring x y c3 c4 c5

        circ xyz =
            coord xyz
                |> (\( x, y ) ->
                        circle
                            [ cx (px x)
                            , cy (px y)
                            , r (px 12)
                            , fill <| locationcolor xyz model
                            , TypedSvg.Events.onClick (Click xyz)
                            ]
                            []
                   )
    in
    svg
        [ viewBox 0 0 1000 400
        ]
        ([ rect
            [ x (px c1)
            , y (px c1)
            , TypedSvg.Attributes.width (px 360)
            , TypedSvg.Attributes.height (px 360)
            , strokeWidth (px 5)
            , noFill
            , stroke black
            ]
            []
         , rect
            [ x (px c2)
            , y (px c2)
            , TypedSvg.Attributes.width (px 240)
            , TypedSvg.Attributes.height (px 240)
            , strokeWidth (px 5)
            , noFill
            , stroke black
            ]
            []
         , rect
            [ x (px c3)
            , y (px c3)
            , TypedSvg.Attributes.width (px 120)
            , TypedSvg.Attributes.height (px 120)
            , strokeWidth (px 5)
            , noFill
            , stroke black
            ]
            []
         , line
            [ x1 (px c4)
            , y1 (px c1)
            , x2 (px c4)
            , y2 (px c3)
            , strokeWidth (px 5)
            , stroke black
            ]
            []
         , line
            [ x1 (px c1)
            , y1 (px c4)
            , x2 (px c3)
            , y2 (px c4)
            , strokeWidth (px 5)
            , stroke black
            ]
            []
         , line
            [ x1 (px c5)
            , y1 (px c4)
            , x2 (px c7)
            , y2 (px c4)
            , strokeWidth (px 5)
            , stroke black
            ]
            []
         , line
            [ x1 (px c4)
            , y1 (px c5)
            , x2 (px c4)
            , y2 (px c7)
            , strokeWidth (px 5)
            , stroke black
            ]
            []
         , TypedSvg.text_
            [ cx (px 200)
            , cy (px 200)
            ]
            [ text "HH" ]
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
            [ Html.Attributes.style
                [ ( "color"
                  , if pass then
                        "darkgreen"

                    else
                        "darkred"
                  )
                ]
            ]
            [ Html.text ("Tests: " ++ toCheck pass)
            ]
        , div [] [ Html.text <| String.join "" (List.map toCheck tests) ]
        ]


viewAlls : Html Msg
viewAlls =
    div [] (List.map (\x -> Html.text <| toString x) allLocations)


toCheck : Bool -> String
toCheck bool =
    case bool of
        True ->
            "✔"

        False ->
            "✘"
