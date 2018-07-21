module Malom exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List exposing (..)


type alias Model =
    { board : Board
    , moveinprogress : MoveInProgress
    , counter : Int
    }


type alias Location =
    ( Row, Column, Ring )


type alias Board =
    List Player


type MoveInProgress
    = NoClick
    | FirstClick Location
    | SecondClick Location Location


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
    ( { board = List.map (always Empty) allLocations, moveinprogress = NoClick, counter = 0 }, Cmd.none )


type Msg
    = Click Location


update : Msg -> Model -> Model
update msg model =
    case msg of
        Click location ->
            if model.counter < 18 then
                case playerOnLocation location model.board of
                    Empty ->
                        case model.moveinprogress of
                            NoClick ->
                                if actualMill location (playerLocations (model.counter |> whoseTurn) model.board) then
                                    { board = occupyLocation (model.counter |> whoseTurn) location model.board
                                    , moveinprogress = FirstClick location
                                    , counter = model.counter
                                    }

                                else
                                    { board = occupyLocation (model.counter |> whoseTurn) location model.board
                                    , moveinprogress = NoClick
                                    , counter = model.counter + 1
                                    }

                            _ ->
                                model

                    _ ->
                        case model.moveinprogress of
                            FirstClick irrelevant ->
                                if isRightElimination location model then
                                    { board = deleteLocation location model.board
                                    , moveinprogress = NoClick
                                    , counter = model.counter + 1
                                    }

                                else
                                    model

                            _ ->
                                model

            else if playerOnLocation location model.board == (whoseTurn <| model.counter) then
                { board = model.board
                , moveinprogress = FirstClick location
                , counter = model.counter
                }

            else if
                playerOnLocation location model.board
                    == Empty
                    && (case model.moveinprogress of
                            FirstClick location ->
                                True

                            _ ->
                                False
                       )
                    && allowedMove (moveInProgressToLocation model.moveinprogress) location
            then
                { board =
                    occupyLocation (model.counter |> whoseTurn)
                        location
                        (deleteLocation
                            (moveInProgressToLocation <|
                                model.moveinprogress
                            )
                            model.board
                        )
                , moveinprogress =
                    SecondClick
                        (moveInProgressToLocation <|
                            model.moveinprogress
                        )
                        location
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


isRightElimination : Location -> Model -> Bool
isRightElimination loc model =
    playerOnLocation loc model.board == (whoseTurn <| model.counter)


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
        (\( y, x, z ) ->
            if any (\h -> h == ( y, x )) [ ( Y1, X1 ), ( Y1, X3 ), ( Y3, X3 ), ( Y3, X1 ) ] then
                Just ( y, x, z )

            else
                Nothing
        )
        allLocations


middles : List Location
middles =
    List.filterMap
        (\( y, x, z ) ->
            if any (\h -> h == ( y, x )) [ ( Y1, X2 ), ( Y2, X1 ), ( Y3, X2 ), ( Y2, X3 ) ] then
                Just ( y, x, z )

            else
                Nothing
        )
        allLocations


allowedMove : Location -> Location -> Bool
allowedMove ( y1, x1, z1 ) ( y2, x2, z2 ) =
    if member ( y1, x1, z1 ) middles then
        if member ( y2, x2, z2 ) middles && ( y1, x1 ) == ( y2, x2 ) then
            True

        else
            ( x1, z1 ) == ( x2, z2 ) || ( y1, z1 ) == ( y2, z2 )

    else
        member ( y2, x2, z2 ) middles && ( x1, z1 ) == ( x2, z2 ) || ( y1, z1 ) == ( y2, z2 )


view : Model -> Html Msg
view model =
    div [ style [ ( "font-family", "monospace" ), ( "font-size", "42px" ) ] ]
        [ viewModel model.board
        , viewAlls

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


tests : List Bool
tests =
    [ update (Click ( Y1, X1, Z1 ))
        { board = [ Empty, B, Empty, Empty, Empty, Empty, Empty, Empty, B, W, Empty, Empty, Empty, Empty, Empty, W, B, Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
        , moveinprogress = NoClick
        , counter = 5
        }
        == { board = [ W, B, Empty, Empty, Empty, Empty, Empty, Empty, B, W, Empty, Empty, Empty, Empty, Empty, W, B, Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
           , moveinprogress = FirstClick ( Y1, X1, Z1 )
           , counter = 5
           }
    , actualMill ( Y1, X1, Z1 ) [ ( Y1, X2, Z1 ), ( Y1, X3, Z1 ) ]
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


viewAlls : Html Msg
viewAlls =
    div [] (List.map (\x -> text <| toString x) allLocations)


allRows : List Row
allRows =
    [ Y1, Y2, Y3 ]


allColumns : List Column
allColumns =
    [ X1, X2, X3 ]


allRings : List Ring
allRings =
    [ Z1, Z2, Z3 ]


allMills : List (List Location)
allMills =
    [ [ ( Y1, X1, Z1 ), ( Y1, X2, Z1 ), ( Y1, X3, Z1 ) ]
    , [ ( Y1, X1, Z1 ), ( Y2, X1, Z1 ), ( Y3, X1, Z1 ) ]
    , [ ( Y3, X1, Z1 ), ( Y3, X2, Z1 ), ( Y3, X3, Z1 ) ]
    , [ ( Y1, X3, Z1 ), ( Y2, X3, Z1 ), ( Y3, X3, Z1 ) ]
    , [ ( Y1, X1, Z2 ), ( Y1, X2, Z2 ), ( Y1, X3, Z2 ) ]
    , [ ( Y1, X1, Z2 ), ( Y2, X1, Z2 ), ( Y3, X1, Z2 ) ]
    , [ ( Y3, X1, Z2 ), ( Y3, X2, Z2 ), ( Y3, X3, Z2 ) ]
    , [ ( Y1, X3, Z2 ), ( Y2, X3, Z2 ), ( Y3, X3, Z2 ) ]
    , [ ( Y1, X1, Z3 ), ( Y1, X2, Z3 ), ( Y1, X3, Z3 ) ]
    , [ ( Y1, X1, Z3 ), ( Y2, X1, Z3 ), ( Y3, X1, Z3 ) ]
    , [ ( Y3, X1, Z3 ), ( Y3, X2, Z3 ), ( Y3, X3, Z3 ) ]
    , [ ( Y1, X3, Z3 ), ( Y2, X3, Z3 ), ( Y3, X3, Z3 ) ]
    , [ ( Y1, X2, Z1 ), ( Y1, X2, Z2 ), ( Y1, X2, Z3 ) ]
    , [ ( Y2, X1, Z1 ), ( Y2, X1, Z2 ), ( Y2, X1, Z3 ) ]
    , [ ( Y3, X2, Z1 ), ( Y3, X2, Z2 ), ( Y3, X2, Z1 ) ]
    , [ ( Y2, X3, Z1 ), ( Y2, X3, Z2 ), ( Y2, X3, Z1 ) ]
    ]



--(r, c, ri)


allLocations : List Location
allLocations =
    List.filterMap
        (\( x, y, z ) ->
            if x == Y2 && y == X2 then
                Nothing

            else
                Just ( x, y, z )
        )
        (concat
            (concat
                (List.map
                    (\c ->
                        List.map
                            (\r ->
                                List.map (\ri -> ( r, c, ri ))
                                    allRings
                            )
                            allRows
                    )
                    allColumns
                )
            )
        )
