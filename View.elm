module View exposing (..)

import Css exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List.Extra
import Morris exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tests exposing (..)


view : Model -> Html Msg
view model =
    div [ Html.Attributes.style [ ( "font-family", "monospace" ), ( "font-size", "42px" ) ] ]
        [ viewB model.board
        , div [ Html.Attributes.style [ ( "transform", "scaleX(2)" ), ( "transform-origin", "0 0" ) ] ]
            [ viewBoard model.board ]

        --, div [] [ Html.text <| playerToString <| winnerPlayer model ]
        , button
            [ onClick Reset ]
            [ Html.text "New Game" ]
        , viewTests
        ]


viewB : Board -> Html Msg
viewB board =
    let
        get n =
            board |> List.Extra.getAt n |> Maybe.withDefault Empty

        loc n =
            viewPlayer (get n) (getLocation n)
    in
    div []
        [ svg [ Svg.Attributes.height "380", Svg.Attributes.width "400" ]
            [ Svg.circle [ cx "20", cy "20", r "12" ] []
            , Svg.circle [ cx "200", cy "20", r "12" ] []
            , Svg.circle [ cx "380", cy "20", r "12" ] []
            , Svg.circle [ cx "380", cy "190", r "12" ] []
            , Svg.circle [ cx "380", cy "360", r "12" ] []
            , Svg.circle [ cx "200", cy "360", r "12" ] []
            , Svg.circle [ cx "20", cy "360", r "12" ] []
            , Svg.circle [ cx "20", cy "190", r "12" ] []
            , rect [ x "20", y "20", Svg.Attributes.width "360", Svg.Attributes.height "340", Svg.Attributes.style "stroke: black; stroke-width: 3; fill: none" ] []
            , Svg.circle [ cx "75", cy "70", r "12" ] []
            , Svg.circle [ cx "200", cy "70", r "12" ] []
            , Svg.circle [ cx "325", cy "70", r "12" ] []
            , Svg.circle [ cx "325", cy "190", r "12" ] []
            , Svg.circle [ cx "325", cy "310", r "12" ] []
            , Svg.circle [ cx "200", cy "310", r "12" ] []
            , Svg.circle [ cx "75", cy "310", r "12" ] []
            , Svg.circle [ cx "75", cy "190", r "12" ] []
            , rect [ x "75", y "70", Svg.Attributes.width "250", Svg.Attributes.height "240", Svg.Attributes.style "stroke: black; stroke-width: 3; fill: none" ] []
            , Svg.circle [ cx "130", cy "120", r "12" ] []
            , Svg.circle [ cx "200", cy "120", r "12" ] []
            , Svg.circle [ cx "270", cy "120", r "12" ] []
            , Svg.circle [ cx "270", cy "190", r "12" ] []
            , Svg.circle [ cx "270", cy "260", r "12" ] []
            , Svg.circle [ cx "200", cy "260", r "12" ] []
            , Svg.circle [ cx "130", cy "260", r "12" ] []
            , Svg.circle [ cx "130", cy "190", r "12" ] []
            , rect [ x "130", y "120", Svg.Attributes.width "140", Svg.Attributes.height "140", Svg.Attributes.style "stroke: black; stroke-width: 3; fill: none" ] []
            , line [ x1 "200", y1 "20", x2 "200", y2 "120", Svg.Attributes.style "stroke: black; stroke-width: 3" ] []
            , line [ x1 "20", y1 "190", x2 "130", y2 "190", Svg.Attributes.style "stroke: black; stroke-width: 3" ] []
            , line [ x1 "200", y1 "360", x2 "200", y2 "260", Svg.Attributes.style "stroke: black; stroke-width: 3" ] []
            , line [ x1 "270", y1 "190", x2 "380", y2 "190", Svg.Attributes.style "stroke: black; stroke-width: 3" ] []
            ]
        ]


viewBoard : Board -> Html Msg
viewBoard board =
    let
        get n =
            board |> List.Extra.getAt n |> Maybe.withDefault Empty

        loc n =
            viewPlayer (get n) (getLocation n)
    in
    div []
        [ div
            []
            [ loc 0
            , Html.text "—"
            , Html.text "—"
            , loc 1
            , Html.text "—"
            , Html.text "—"
            , loc 2
            ]
        , div []
            [ Html.text "│"
            , loc 3
            , Html.text "—"
            , loc 4
            , Html.text "—"
            , loc 5
            , Html.text "│"
            ]
        , div []
            [ Html.text "│"
            , Html.text "│"
            , loc 6
            , loc 7
            , loc 8
            , Html.text "│"
            , Html.text "│"
            ]
        , div []
            [ loc 9
            , loc 10
            , loc 11
            , Html.text " "
            , loc 12
            , loc 13
            , loc 14
            ]
        , div []
            [ Html.text "│"
            , Html.text "│"
            , loc 15
            , loc 16
            , loc 17
            , Html.text "│"
            , Html.text "│"
            ]
        , div []
            [ Html.text "│"
            , loc 18
            , Html.text "—"
            , loc 19
            , Html.text "—"
            , loc 20
            , Html.text "│"
            ]
        , div []
            [ loc 21
            , Html.text "—"
            , Html.text "—"
            , loc 22
            , Html.text "—"
            , Html.text "—"
            , loc 23
            ]
        ]


playerToPiece : Player -> Location -> Html Msg
playerToPiece player loc =
    case player of
        Empty ->
            Html.text "·"

        W ->
            img [ src "2.jpg", Html.Attributes.width 23, Html.Attributes.height 30 ] []

        B ->
            img [ src "1.jpg", Html.Attributes.width 23, Html.Attributes.height 30 ] []


viewPlayer : Player -> Location -> Html Msg
viewPlayer player loc =
    span
        [ onClick (Click loc)
        , Html.Attributes.style
            [ ( "color", "red" )
            ]
        ]
        [ --Html.text (playerToString player)
          playerToPiece player loc

        --, span [ style [ ( "font-size", "0.3em" ) ] ] [ Html.text (loc |> (\( y, x, z ) -> toString y ++ toString x ++ toString z)) ]
        ]


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
