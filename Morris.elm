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


{-| It should always be 24 players long.
-}
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
    | Reset


allColumns : List Column
allColumns =
    [ X1, X2, X3 ]


allRows : List Row
allRows =
    [ Y1, Y2, Y3 ]


allRings : List Ring
allRings =
    [ Z1, Z2, Z3 ]


{-| All mills possible.
-}
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


{-| All the locations.
-}
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


{-| All the locations located on the corners of the board.
-}
corners : List Location
corners =
    allLocations
        |> List.filterMap
            (\( x, y, z ) ->
                maybe
                    (any
                        (\h ->
                            h == ( x, y )
                        )
                        [ ( X1, Y1 ), ( X3, Y1 ), ( X3, Y3 ), ( X1, Y3 ) ]
                    )
                    ( x, y, z )
            )


{-| All the locations located in the middle of the board.
-}
middles : List Location
middles =
    allLocations
        |> List.filterMap
            (\( x, y, z ) ->
                maybe
                    (any
                        (\h ->
                            h == ( x, y )
                        )
                        [ ( X2, Y1 ), ( X1, Y2 ), ( X2, Y3 ), ( X3, Y2 ) ]
                    )
                    ( x, y, z )
            )


{-| The initial model.
-}
init : ( Model, Cmd Msg )
init =
    ( { board = List.map (always Empty) allLocations, moveinprogress = NoClick, counter = 0 }, Cmd.none )


{-| Based on the actions of the user updates the model.
-}
update : Msg -> Model -> Model
update msg model =
    let
        currentplayer =
            model.counter |> whoseTurn

        isfirstclick =
            case model.moveinprogress of
                FirstClick _ ->
                    True

                _ ->
                    False

        issecondclick =
            case model.moveinprogress of
                SecondClick _ _ ->
                    True

                _ ->
                    False
    in
    case msg of
        Click location ->
            if model.counter < 18 then
                case playerOnLocation location model.board of
                    Empty ->
                        case model.moveinprogress of
                            NoClick ->
                                { board = updateBoard currentplayer location model.board
                                , moveinprogress =
                                    if isNewMill location <| playerLocations currentplayer model.board then
                                        FirstClick location

                                    else
                                        NoClick
                                , counter =
                                    if isNewMill location <| playerLocations currentplayer model.board then
                                        model.counter

                                    else
                                        model.counter + 1
                                }

                            _ ->
                                model

                    _ ->
                        if isfirstclick then
                            if validElimination location model then
                                { board = deleteLocation location model.board
                                , moveinprogress = NoClick
                                , counter = model.counter + 1
                                }

                            else
                                model

                        else
                            model

            else if isWin model then
                model

            else
                case playerOnLocation location model.board == currentplayer of
                    True ->
                        { model | moveinprogress = FirstClick location }

                    False ->
                        case playerOnLocation location model.board of
                            Empty ->
                                if length (playerLocations currentplayer model.board) == 3 then
                                    if
                                        isfirstclick
                                            && isNewMill location (playerLocations currentplayer (deleteLocation (moveInProgressToLocation <| model.moveinprogress) model.board))
                                    then
                                        { board = updateBoard currentplayer location (deleteLocation (moveInProgressToLocation <| model.moveinprogress) model.board)
                                        , moveinprogress = SecondClick (moveInProgressToLocation model.moveinprogress) location
                                        , counter = model.counter
                                        }

                                    else if isfirstclick then
                                        { board = updateBoard currentplayer location (deleteLocation (moveInProgressToLocation <| model.moveinprogress) model.board)
                                        , moveinprogress = NoClick
                                        , counter = model.counter + 1
                                        }

                                    else
                                        model

                                else if
                                    isfirstclick
                                        && allowedMove location (moveInProgressToLocation model.moveinprogress)
                                        && isNewMill location (playerLocations currentplayer (deleteLocation (moveInProgressToLocation <| model.moveinprogress) model.board))
                                then
                                    { board = updateBoard currentplayer location (deleteLocation (moveInProgressToLocation <| model.moveinprogress) model.board)
                                    , moveinprogress = SecondClick (moveInProgressToLocation model.moveinprogress) location
                                    , counter = model.counter
                                    }

                                else if
                                    isfirstclick
                                        && allowedMove location (moveInProgressToLocation model.moveinprogress)
                                then
                                    { board = updateBoard currentplayer location (deleteLocation (moveInProgressToLocation <| model.moveinprogress) model.board)
                                    , moveinprogress = NoClick
                                    , counter = model.counter + 1
                                    }

                                else
                                    model

                            _ ->
                                if
                                    issecondclick
                                        && validElimination location model
                                then
                                    { board = deleteLocation location model.board
                                    , moveinprogress = NoClick
                                    , counter = model.counter + 1
                                    }

                                else
                                    model

        Reset ->
            Tuple.first init


{-| Return True if a player has no legal moves or has < 2 players.
-}
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
                        (\playerloc ->
                            List.map
                                (\emptyloc -> allowedMove playerloc emptyloc)
                                (playerLocations Empty model.board)
                        )
                        (playerLocations W model.board)
                )
            )
        || not
            (any
                identity
                (concat <|
                    (playerLocations B model.board
                        |> List.map
                            (\playerloc ->
                                List.map
                                    (\emptyloc -> allowedMove playerloc emptyloc)
                                    (playerLocations Empty model.board)
                            )
                    )
                )
            )


{-| Defines if the location is in a mill, in the initial phases of the game.
-}
isNewMill : Location -> List Location -> Bool
isNewMill location playerlocations =
    any
        (\mill -> member location mill)
        (allMills
            |> List.filterMap
                (\mill ->
                    maybe
                        (all (\loc -> member loc ([ location ] ++ playerlocations)) mill)
                        mill
                )
        )


{-| Defines if the location is in a mill, when all the pieces are on the board.
-}
isActiveMill : Location -> List Location -> Bool
isActiveMill location playerlocations =
    any
        (\mill -> all (\loc -> member loc playerlocations) mill)
        (allMills
            |> List.filterMap
                (\mill -> maybe (member location mill) mill)
        )


{-| Defines if the desired elimination is valid.
-}
validElimination : Location -> Model -> Bool
validElimination loc model =
    all
        (\playerloc ->
            any
                (\mill -> member playerloc mill && all (\loc -> member loc (playerLocations (whoseTurn <| model.counter + 1) model.board)) mill)
                allMills
        )
        (playerLocations (whoseTurn <| model.counter + 1) model.board)
        || (playerOnLocation loc model.board == (whoseTurn <| model.counter + 1))
        && not
            (isActiveMill loc <| playerLocations (whoseTurn <| model.counter + 1) model.board)


{-| Deleting a location from the board. This function is useful when the player moves from one location to another.
-}
deleteLocation : Location -> Board -> Board
deleteLocation loc board =
    updateAt (locationIndex loc) (\player -> Empty) board


{-| Stored location to location.
-}
moveInProgressToLocation : MoveInProgress -> Location
moveInProgressToLocation mip =
    case mip of
        FirstClick loc ->
            loc

        _ ->
            ( X1, Y1, Z1 )


{-| Returns True or False if it's possible to move a player from one location to another.
-}
allowedMove : Location -> Location -> Bool
allowedMove ( x1, y1, z1 ) ( x2, y2, z2 ) =
    let
        isfirstmiddle =
            member ( x1, y1, z1 ) middles

        issecondmiddle =
            member ( x2, y2, z2 ) middles
    in
    case isfirstmiddle of
        True ->
            case issecondmiddle of
                True ->
                    if (z1 == Z1 && z2 == Z3) || (z1 == Z3 && z2 == Z1) then
                        False

                    else
                        ( x1, y1 ) == ( x2, y2 )

                False ->
                    ( x1, z1 ) == ( x2, z2 ) || ( y1, z1 ) == ( y2, z2 )

        False ->
            case issecondmiddle of
                True ->
                    ( x1, z1 ) == ( x2, z2 ) || ( y1, z1 ) == ( y2, z2 )

                False ->
                    False


{-| Turns a player to a string.
-}
playerToString : Player -> String
playerToString player =
    case player of
        Empty ->
            "·"

        W ->
            "W"

        B ->
            "B"


{-| Returns the corresponding index for a location.
-}
locationIndex : Location -> Int
locationIndex loc =
    case
        elemIndex loc allLocations
    of
        Just a ->
            a

        _ ->
            0


{-| Returns True if the player has a mill.
-}
isMill : List Location -> Bool
isMill playerlocations =
    any
        identity
        (List.map
            (\mm ->
                List.length mm == 3
            )
            (allMills
                |> List.map
                    (\x ->
                        List.filterMap
                            (\loc -> maybe (member loc playerlocations) loc)
                            x
                    )
            )
        )


{-| Returns the list of locations where the given player has pieces on the board.
-}
playerLocations : Player -> Board -> List Location
playerLocations player board =
    allLocations
        |> List.filterMap
            (\loc ->
                maybe (playerOnLocation loc board == player) loc
            )


{-| Returns the player on the location.
-}
playerOnLocation : Location -> Board -> Player
playerOnLocation loc board =
    case getAt (locationIndex loc) board of
        Just a ->
            a

        _ ->
            Empty


{-| Updates the board with the current player on the desired position/location.
-}
updateBoard : Player -> Location -> Board -> Board
updateBoard player loc board =
    List.take (locationIndex loc) board ++ [ player ] ++ List.drop (locationIndex loc + 1) board


{-| Based on the game round returns a player.
-}
whoseTurn : Int -> Player
whoseTurn x =
    if x % 2 == 0 then
        W

    else
        B


{-| Returns the corresponding location for an index.
-}
getLocation : Int -> Location
getLocation i =
    case getAt i allLocations of
        Just a ->
            a

        Nothing ->
            ( X1, Y1, Z1 )


{-| Helper function.
-}
maybe : Bool -> a -> Maybe a
maybe bool a =
    if bool then
        Just a

    else
        Nothing
