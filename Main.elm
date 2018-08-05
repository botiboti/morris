module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List exposing (..)
import List.Extra
import Types exposing (..)


init : ( Model, Cmd Msg )
init =
    ( { board = List.map (always Empty) allLocations, moveinprogress = NoClick, counter = 0 }, Cmd.none )


update : Msg -> Model -> Model
update msg model =
    case msg of
        Click location ->
            if model.counter < 18 then
                case playerOnLocation location model.board of
                    Empty ->
                        case model.moveinprogress of
                            NoClick ->
                                { board = occupyLocation (model.counter |> whoseTurn) location model.board
                                , moveinprogress =
                                    if actualMill location <| playerLocations (model.counter |> whoseTurn) model.board then
                                        FirstClick location

                                    else
                                        NoClick
                                , counter =
                                    if actualMill location <| playerLocations (model.counter |> whoseTurn) model.board then
                                        model.counter

                                    else
                                        model.counter + 1
                                }

                            _ ->
                                model

                    _ ->
                        case model.moveinprogress of
                            FirstClick irrelevant ->
                                if isRightEliminationm location model then
                                    { board = deleteLocation location model.board
                                    , moveinprogress = NoClick
                                    , counter = model.counter + 1
                                    }

                                else
                                    model

                            _ ->
                                model

            else
                case playerOnLocation location model.board == (whoseTurn <| model.counter) of
                    True ->
                        { board = model.board
                        , moveinprogress = FirstClick location
                        , counter = model.counter
                        }

                    False ->
                        case playerOnLocation location model.board of
                            Empty ->
                                if length (playerLocations (model.counter |> whoseTurn) model.board) == 3 then
                                    if
                                        (case model.moveinprogress of
                                            FirstClick x ->
                                                True

                                            _ ->
                                                False
                                        )
                                            && isNewMill location (playerLocations (model.counter |> whoseTurn) (deleteLocation (moveInProgressToLocation <| model.moveinprogress) model.board))
                                    then
                                        { board = occupyLocation (model.counter |> whoseTurn) location (deleteLocation (moveInProgressToLocation <| model.moveinprogress) model.board)
                                        , moveinprogress = SecondClick (moveInProgressToLocation model.moveinprogress) location
                                        , counter = model.counter
                                        }

                                    else if
                                        case model.moveinprogress of
                                            FirstClick x ->
                                                True

                                            _ ->
                                                False
                                    then
                                        { board = occupyLocation (model.counter |> whoseTurn) location (deleteLocation (moveInProgressToLocation <| model.moveinprogress) model.board)
                                        , moveinprogress = NoClick
                                        , counter = model.counter + 1
                                        }

                                    else
                                        model

                                else if
                                    (case model.moveinprogress of
                                        FirstClick x ->
                                            True

                                        _ ->
                                            False
                                    )
                                        && allowedMove location (moveInProgressToLocation model.moveinprogress) model
                                        && isNewMill location (playerLocations (model.counter |> whoseTurn) (deleteLocation (moveInProgressToLocation <| model.moveinprogress) model.board))
                                then
                                    { board = occupyLocation (model.counter |> whoseTurn) location (deleteLocation (moveInProgressToLocation <| model.moveinprogress) model.board)
                                    , moveinprogress = SecondClick (moveInProgressToLocation model.moveinprogress) location
                                    , counter = model.counter
                                    }

                                else if
                                    (case model.moveinprogress of
                                        FirstClick x ->
                                            True

                                        _ ->
                                            False
                                    )
                                        && allowedMove location (moveInProgressToLocation model.moveinprogress) model
                                then
                                    { board = occupyLocation (model.counter |> whoseTurn) location (deleteLocation (moveInProgressToLocation <| model.moveinprogress) model.board)
                                    , moveinprogress = NoClick
                                    , counter = model.counter + 1
                                    }

                                else
                                    model

                            _ ->
                                if
                                    (case model.moveinprogress of
                                        SecondClick x y ->
                                            True

                                        _ ->
                                            False
                                    )
                                        && isRightEliminationm location model
                                then
                                    { board = deleteLocation location model.board
                                    , moveinprogress = NoClick
                                    , counter = model.counter + 1
                                    }

                                else
                                    model


actualMill : Location -> List Location -> Bool
actualMill location playerlocations =
    any
        (\mill -> all (\loc -> member loc ([ location ] ++ playerlocations)) mill)
        (List.filterMap
            (\mill ->
                if member location mill then
                    Just mill

                else
                    Nothing
            )
            allMills
        )


isNewMill : Location -> List Location -> Bool
isNewMill location playerlocations =
    any
        (\mill -> member location mill)
        (List.filterMap
            (\mill ->
                if all (\loc -> member loc ([ location ] ++ playerlocations)) mill then
                    Just mill

                else
                    Nothing
            )
            allMills
        )


actualMillModed : Location -> List Location -> Bool
actualMillModed location playerlocations =
    any
        (\mill -> all (\loc -> member loc playerlocations) mill)
        (List.filterMap
            (\mill ->
                if member location mill then
                    Just mill

                else
                    Nothing
            )
            allMills
        )


isRightElimination : Location -> Model -> Bool
isRightElimination loc model =
    playerOnLocation loc model.board == (whoseTurn <| model.counter)


isRightEliminationm : Location -> Model -> Bool
isRightEliminationm loc model =
    (playerOnLocation loc model.board == (whoseTurn <| model.counter + 1)) && not (actualMillModed loc <| playerLocations (whoseTurn <| model.counter + 1) model.board)


deleteLocation : Location -> Board -> Board
deleteLocation loc board =
    List.take (locationIndex loc) board ++ [ Empty ] ++ List.drop (locationIndex loc + 1) board


moveInProgressToLocation : MoveInProgress -> Location
moveInProgressToLocation mip =
    case mip of
        FirstClick loc ->
            loc

        _ ->
            Debug.crash "HAHA"


corners : List Location
corners =
    List.filterMap
        (\( x, y, z ) ->
            if any (\h -> h == ( x, y )) [ ( X1, Y1 ), ( X3, Y1 ), ( X3, Y3 ), ( X1, Y3 ) ] then
                Just ( x, y, z )

            else
                Nothing
        )
        allLocations


middles : List Location
middles =
    List.filterMap
        (\( x, y, z ) ->
            if any (\h -> h == ( x, y )) [ ( X2, Y1 ), ( X1, Y2 ), ( X2, Y3 ), ( X3, Y2 ) ] then
                Just ( x, y, z )

            else
                Nothing
        )
        allLocations


allowedMove : Location -> Location -> Model -> Bool
allowedMove ( x1, y1, z1 ) ( x2, y2, z2 ) model =
    case member ( x1, y1, z1 ) middles of
        True ->
            case member ( x2, y2, z2 ) middles of
                True ->
                    if (z1 == Z1 && z2 == Z3) || (z1 == Z3 && z2 == Z1) then
                        False

                    else
                        ( x1, y1 ) == ( x2, y2 )

                False ->
                    ( x1, z1 ) == ( x2, z2 ) || ( y1, z1 ) == ( y2, z2 )

        False ->
            member ( x2, y2, z2 ) middles && ( x1, z1 ) == ( x2, z2 ) || ( y1, z1 ) == ( y2, z2 )


view : Model -> Html Msg
view model =
    div [ style [ ( "font-family", "monospace" ), ( "font-size", "42px" ) ] ]
        [ --viewModel model.board
          div [ style [ ( "transform", "scaleX(2)" ), ( "transform-origin", "0 0" ) ] ] [ viewBoard model.board ]

        --, div [] [ text (toString model.moveinprogress) ]
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


viewPlayer : Player -> Location -> Html Msg
viewPlayer player loc =
    span
        [ onClick (Click loc)
        , style
            [ ( "color", "black" )
            ]
        ]
        [ --text (playerToString player)
          playerToPiece player

        --, span [ style [ ( "font-size", "0.3em" ) ] ] [ text (loc |> (\( y, x, z ) -> toString y ++ toString x ++ toString z)) ]
        ]


playerToPiece : Player -> Html Msg
playerToPiece player =
    case player of
        Empty ->
            text "·"

        W ->
            img [ src "2.jpg", width 23, height 30 ] []

        B ->
            img [ src "1.jpg", width 23, height 30 ] []


getLocation : Int -> Location
getLocation i =
    case List.head (List.drop i allLocations) of
        Just a ->
            a

        Nothing ->
            ( X1, Y1, Z1 )


playerToString : Player -> String
playerToString player =
    case player of
        Empty ->
            "·"

        W ->
            "W"

        B ->
            "B"


locationIndex : Location -> Int
locationIndex loc =
    allLocations
        |> List.indexedMap (,)
        |> List.filterMap
            (\( i, l ) ->
                if l == loc then
                    Just i

                else
                    Nothing
            )
        |> List.head
        |> unwrap


isMill : List Location -> Bool
isMill playerlocations =
    any
        (\aa -> aa == True)
        (List.map
            (\mm ->
                if List.length mm == 3 then
                    True

                else
                    False
            )
            (List.map
                (\x ->
                    List.filterMap
                        (\loc ->
                            if member loc playerlocations then
                                Just loc

                            else
                                Nothing
                        )
                        x
                )
                allMills
            )
        )


playerLocations : Player -> Board -> List Location
playerLocations player board =
    List.filterMap
        (\loc ->
            if playerOnLocation loc board == player then
                Just loc

            else
                Nothing
        )
        allLocations


playerOnLocation : Location -> Board -> Player
playerOnLocation loc board =
    unwrap (List.head (List.drop (locationIndex loc) board))


occupyLocation : Player -> Location -> Board -> Board
occupyLocation player loc board =
    List.take (locationIndex loc) board ++ [ player ] ++ List.drop (locationIndex loc + 1) board


unwrap : Maybe a -> a
unwrap a =
    case a of
        Just a ->
            a

        Nothing ->
            Debug.crash "HAHA"


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


mkBoard : List Location -> List Location -> List Player
mkBoard ws bs =
    repeat 24 Empty
        |> indexedMap
            (\i l ->
                if member (getLocation i) ws then
                    W

                else if member (getLocation i) bs then
                    B

                else
                    Empty
            )


tests : List Bool
tests =
    let
        m1 =
            { board = mkBoard [ ( X2, Y1, Z1 ), ( X3, Y1, Z1 ) ] [ ( X3, Y1, Z3 ), ( X3, Y2, Z1 ) ]
            , moveinprogress = NoClick
            , counter = 4
            }

        m2 =
            update (Click ( X1, Y1, Z1 )) m1

        m3 =
            update (Click ( X3, Y1, Z3 )) m2
    in
    [ m2
        == { board = mkBoard [ ( X1, Y1, Z1 ), ( X2, Y1, Z1 ), ( X3, Y1, Z1 ) ] [ ( X3, Y1, Z3 ), ( X3, Y2, Z1 ) ]
           , moveinprogress = FirstClick ( X1, Y1, Z1 )
           , counter = 4
           }
    , m3
        == { board = mkBoard [ ( X1, Y1, Z1 ), ( X2, Y1, Z1 ), ( X3, Y1, Z1 ) ] [ ( X3, Y2, Z1 ) ]
           , moveinprogress = NoClick
           , counter = 5
           }
    , actualMill ( X1, Y1, Z1 ) [ ( X2, Y1, Z1 ), ( X3, Y1, Z1 ) ]
    , actualMill ( X2, Y3, Z2 ) [ ( X3, Y3, Z2 ), ( X2, Y3, Z1 ) ] == False

    --, allowedMove ( X1, Y2, Z2 ) ( X3, Y2, Z2 ) == False
    --, allowedMove ( X2, Y1, Z3 ) ( X2, Y3, Z3 ) == False
    --, allowedMove ( X2, Y1, Z3 ) ( X2, Y1, Z2 )
    --, allowedMove ( X1, Y1, Z1 ) ( X3, Y3, Z1 ) == False
    --, allowedMove ( X1, Y2, Z3 ) ( X1, Y1, Z3 )
    , always False "ha 3 marad, ugral"
    , always False "win detection (<2 pieces, blocked)"
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


toCheck : Bool -> String
toCheck bool =
    case bool of
        True ->
            "✔"

        False ->
            "✘"


viewAlls : Html Msg
viewAlls =
    div [] (List.map (\x -> text <| toString x) allLocations)
