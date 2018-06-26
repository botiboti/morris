module Malom exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List exposing (..)


type alias Model =
    { board : Board
    , counter : Int
    }


type alias Location =
    ( Row, Column, Ring )


type alias Board =
    List Player


type Column
    = X1
    | X2
    | X3


type Row
    = Y1
    | Y2
    | Y3


type Ring
    = Z1
    | Z2
    | Z3


type Player
    = Empty
    | W
    | B


init : ( Model, Cmd Msg )
init =
    ( { board = List.map (always Empty) allLocations, counter = 0 }, Cmd.none )


type Msg
    = Click Location


update : Msg -> Model -> Model
update msg model =
    case msg of
        Click location ->
            { board = occupyLocation (model.counter |> whoseTurn) location model.board, counter = model.counter + 1 }


view : Model -> Html Msg
view model =
    div [ style [ ( "font-family", "monospace" ), ( "font-size", "42px" ) ] ]
        [ viewModel model.board

        --, div [] [ text <| playerToString <| winnerPlayer model ]
        --, button [ onClick Reset ] [ text "New Game" ]
        , viewTests
        ]


viewModel : Board -> Html Msg
viewModel board =
    div [] (List.indexedMap (\i p -> viewPlayer p (getLocation i)) board)


viewPlayer : Player -> Location -> Html Msg
viewPlayer player loc =
    span
        [ onClick (Click loc)
        , style
            [ ( "border-right", "2px solid black" )
            , ( "border-bottom", "2px solid black" )
            , ( "color"
              , "black"
              )
            ]
        ]
        [ text
            (playerToString <| player)
        ]


getLocation : Int -> Location
getLocation i =
    case List.head (List.drop i allLocations) of
        Just a ->
            a

        Nothing ->
            Debug.crash "HAHA"


playerToString : Player -> String
playerToString player =
    case player of
        Empty ->
            "_"

        W ->
            "W"

        B ->
            "B"


eliminatePlayer : Location -> Model -> Model
eliminatePlayer loc model =
    -- TODO
    model


occupyLocation : Player -> Location -> Board -> Board
occupyLocation player loc board =
    let
        f loc =
            allLocations
                |> List.indexedMap (,)
                |> List.filterMap
                    (\( i, l ) ->
                        if l == loc then
                            Just i

                        else
                            Nothing
                    )
    in
    case f loc of
        x :: xs ->
            List.take x board ++ [ player ] ++ List.drop (x + 1) board

        [] ->
            board


whoseTurn : Int -> Player
whoseTurn x =
    if x % 2 == 0 then
        W

    else
        B


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        }


tests : List Bool
tests =
    []


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
            [ text
                ("Tests: "
                    ++ (if pass then
                            "✔"

                        else
                            "✘"
                       )
                )
            ]
        , div [] [ text <| toString tests ]
        ]


allRows : List Row
allRows =
    [ Y1, Y2, Y3 ]


allColumns : List Column
allColumns =
    [ X1, X2, X3 ]


allRings : List Ring
allRings =
    [ Z1, Z2, Z3 ]



--(r, c, ri)


allLocations : List Location
allLocations =
    concat
        (concat
            (List.map
                (\r ->
                    List.map
                        (\c ->
                            List.map (\ri -> ( r, c, ri ))
                                allRings
                        )
                        allColumns
                )
                allRows
            )
        )
