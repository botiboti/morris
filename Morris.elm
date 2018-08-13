module Morris exposing (..)

import List exposing (..)
import List.Extra exposing (..)


type alias Model =
    { board : Board
    , moveinprogress : MoveInProgress
    , counter : Int
    }


type alias Location =
    ( Column, Row, Ring )


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


type Msg
    = Click Location


allColumns : List Column
allColumns =
    [ X1, X2, X3 ]


allRows : List Row
allRows =
    [ Y1, Y2, Y3 ]


allRings : List Ring
allRings =
    [ Z1, Z2, Z3 ]


allMills : List (List Location)
allMills =
    [ [ ( X1, Y1, Z1 ), ( X2, Y1, Z1 ), ( X3, Y1, Z1 ) ]
    , [ ( X1, Y1, Z1 ), ( X1, Y2, Z1 ), ( X1, Y3, Z1 ) ]
    , [ ( X1, Y3, Z1 ), ( X2, Y3, Z1 ), ( X3, Y3, Z1 ) ]
    , [ ( X3, Y1, Z1 ), ( X3, Y2, Z1 ), ( X3, Y3, Z1 ) ]
    , [ ( X1, Y1, Z2 ), ( X2, Y1, Z2 ), ( X3, Y1, Z2 ) ]
    , [ ( X1, Y1, Z2 ), ( X1, Y2, Z2 ), ( X1, Y3, Z2 ) ]
    , [ ( X1, Y3, Z2 ), ( X2, Y3, Z2 ), ( X3, Y3, Z2 ) ]
    , [ ( X3, Y1, Z2 ), ( X3, Y2, Z2 ), ( X3, Y3, Z2 ) ]
    , [ ( X1, Y1, Z3 ), ( X2, Y1, Z3 ), ( X3, Y1, Z3 ) ]
    , [ ( X1, Y1, Z3 ), ( X1, Y2, Z3 ), ( X1, Y3, Z3 ) ]
    , [ ( X1, Y3, Z3 ), ( X2, Y3, Z3 ), ( X3, Y3, Z3 ) ]
    , [ ( X3, Y1, Z3 ), ( X3, Y2, Z3 ), ( X3, Y3, Z3 ) ]
    , [ ( X2, Y1, Z1 ), ( X2, Y1, Z2 ), ( X2, Y1, Z3 ) ]
    , [ ( X1, Y2, Z1 ), ( X1, Y2, Z2 ), ( X1, Y2, Z3 ) ]
    , [ ( X2, Y3, Z1 ), ( X2, Y3, Z2 ), ( X2, Y3, Z3 ) ]
    , [ ( X3, Y2, Z1 ), ( X3, Y2, Z2 ), ( X3, Y2, Z3 ) ]
    ]


allLocations : List Location
allLocations =
    [ ( X1, Y1, Z1 )
    , ( X2, Y1, Z1 )
    , ( X3, Y1, Z1 )
    , ( X1, Y1, Z2 )
    , ( X2, Y1, Z2 )
    , ( X3, Y1, Z2 )
    , ( X1, Y1, Z3 )
    , ( X2, Y1, Z3 )
    , ( X3, Y1, Z3 )
    , ( X1, Y2, Z1 )
    , ( X1, Y2, Z2 )
    , ( X1, Y2, Z3 )
    , ( X3, Y2, Z3 )
    , ( X3, Y2, Z2 )
    , ( X3, Y2, Z1 )
    , ( X1, Y3, Z3 )
    , ( X2, Y3, Z3 )
    , ( X3, Y3, Z3 )
    , ( X1, Y3, Z2 )
    , ( X2, Y3, Z2 )
    , ( X3, Y3, Z2 )
    , ( X1, Y3, Z1 )
    , ( X2, Y3, Z1 )
    , ( X3, Y3, Z1 )
    ]


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
                                    if isNewMill location <| playerLocations (model.counter |> whoseTurn) model.board then
                                        FirstClick location

                                    else
                                        NoClick
                                , counter =
                                    if isNewMill location <| playerLocations (model.counter |> whoseTurn) model.board then
                                        model.counter

                                    else
                                        model.counter + 1
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

            else if isWin model then
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
                                        && allowedMove location (moveInProgressToLocation model.moveinprogress)
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
                                        && allowedMove location (moveInProgressToLocation model.moveinprogress)
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
                                        && isRightElimination location model
                                then
                                    { board = deleteLocation location model.board
                                    , moveinprogress = NoClick
                                    , counter = model.counter + 1
                                    }

                                else
                                    model


isWin : Model -> Bool
isWin model =
    any
        identity
        (List.map
            (\player -> length (playerLocations player model.board) < 3)
            [ W, B ]
        )
        || not
            (any
                identity
                (concat <|
                    List.map
                        (\playerloc -> List.map (\emptyloc -> allowedMove playerloc emptyloc) (playerLocations Empty model.board))
                        (playerLocations W model.board)
                )
            )
        || not
            (any
                identity
                (concat <|
                    List.map
                        (\playerloc -> List.map (\emptyloc -> allowedMove playerloc emptyloc) (playerLocations Empty model.board))
                        (playerLocations B model.board)
                )
            )


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
    all
        (\playerloc -> any (\mill -> member playerloc mill && all (\loc -> member loc (playerLocations (whoseTurn <| model.counter + 1) model.board)) mill) allMills)
        (playerLocations (whoseTurn <| model.counter + 1) model.board)
        || (playerOnLocation loc model.board == (whoseTurn <| model.counter + 1))
        && not (actualMillModed loc <| playerLocations (whoseTurn <| model.counter + 1) model.board)


deleteLocation : Location -> Board -> Board
deleteLocation loc board =
    List.take (locationIndex loc) board ++ [ Empty ] ++ List.drop (locationIndex loc + 1) board


moveInProgressToLocation : MoveInProgress -> Location
moveInProgressToLocation mip =
    case mip of
        FirstClick loc ->
            loc

        _ ->
            ( X1, Y1, Z1 )


allowedMove : Location -> Location -> Bool
allowedMove ( x1, y1, z1 ) ( x2, y2, z2 ) =
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
            case member ( x2, y2, z2 ) middles of
                True ->
                    ( x1, z1 ) == ( x2, z2 ) || ( y1, z1 ) == ( y2, z2 )

                False ->
                    False


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
    case
        elemIndex loc allLocations
    of
        Just a ->
            a

        _ ->
            0


isMill : List Location -> Bool
isMill playerlocations =
    any
        identity
        (List.map
            (\mm ->
                List.length mm == 3
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
    case getAt (locationIndex loc) board of
        Just a ->
            a

        _ ->
            Empty


occupyLocation : Player -> Location -> Board -> Board
occupyLocation player loc board =
    List.take (locationIndex loc) board ++ [ player ] ++ List.drop (locationIndex loc + 1) board


whoseTurn : Int -> Player
whoseTurn x =
    if x % 2 == 0 then
        W

    else
        B


getLocation : Int -> Location
getLocation i =
    case getAt i allLocations of
        Just a ->
            a

        Nothing ->
            ( X1, Y1, Z1 )
