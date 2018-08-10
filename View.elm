module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List.Extra
import Morris exposing (..)
import Tests exposing (..)


view : Model -> Html Msg
view model =
    div [ style [ ( "font-family", "monospace" ), ( "font-size", "42px" ) ] ]
        [ --viewModel model.board
          div [ style [ ( "transform", "scaleX(2)" ), ( "transform-origin", "0 0" ) ] ]
            [ viewBoard model.board ]

        --, viewAlls
        --, div [] [ text <| playerToString <| winnerPlayer model ]
        --, button [ onClick Reset ] [ text "New Game" ]
        , viewTests
        ]


viewModel : Board -> Html Msg
viewModel board =
    div [] (List.indexedMap (\i p -> viewPlayer p (getLocation i)) board)


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
            , text "—"
            , text "—"
            , loc 1
            , text "—"
            , text "—"
            , loc 2
            ]
        , div []
            [ text "│"
            , loc 3
            , text "—"
            , loc 4
            , text "—"
            , loc 5
            , text "│"
            ]
        , div []
            [ text "│"
            , text "│"
            , loc 6
            , loc 7
            , loc 8
            , text "│"
            , text "│"
            ]
        , div []
            [ loc 9
            , loc 10
            , loc 11
            , text " "
            , loc 12
            , loc 13
            , loc 14
            ]
        , div []
            [ text "│"
            , text "│"
            , loc 15
            , loc 16
            , loc 17
            , text "│"
            , text "│"
            ]
        , div []
            [ text "│"
            , loc 18
            , text "—"
            , loc 19
            , text "—"
            , loc 20
            , text "│"
            ]
        , div []
            [ loc 21
            , text "—"
            , text "—"
            , loc 22
            , text "—"
            , text "—"
            , loc 23
            ]
        ]


playerToPiece : Player -> Location -> Html Msg
playerToPiece player loc =
    case player of
        Empty ->
            text "·"

        W ->
            img [ src "2.jpg", width 23, height 30 ] []

        B ->
            img [ src "1.jpg", width 23, height 30 ] []


viewPlayer : Player -> Location -> Html Msg
viewPlayer player loc =
    span
        [ onClick (Click loc)
        , style
            [ ( "color", "black" )
            ]
        ]
        [ --text (playerToString player)
          playerToPiece player loc

        --, span [ style [ ( "font-size", "0.3em" ) ] ] [ text (loc |> (\( y, x, z ) -> toString y ++ toString x ++ toString z)) ]
        ]


viewTests : Html Msg
viewTests =
    let
        pass =
            List.all identity tests
    in
    div []
        [ h3
            [ style
                [ ( "color"
                  , if pass then
                        "darkgreen"

                    else
                        "darkred"
                  )
                ]
            ]
            [ text ("Tests: " ++ toCheck pass)
            ]
        , div [] [ text <| String.join "" (List.map toCheck tests) ]
        ]


viewAlls : Html Msg
viewAlls =
    div [] (List.map (\x -> text <| toString x) allLocations)


toCheck : Bool -> String
toCheck bool =
    case bool of
        True ->
            "✔"

        False ->
            "✘"
