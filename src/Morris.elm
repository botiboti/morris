module Morris exposing (..)

import Animation exposing (..)
import Ease exposing (..)
import List exposing (..)
import List.Extra exposing (..)
import Time exposing (..)


type alias Model =
    { board : Board
    , moveInProgress : MoveInProgress
    , counter : Int
    , style : Animation.State
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
    | Animate Animation.Msg
    | Reset


type Phase
    = Place
    | Move
    | Fly


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


{-| All the corners of the board.
-}
corners : List Location
corners =
    allLocations
        |> List.filter
            (\( x, y, z ) ->
                member ( x, y ) [ ( X1, Y1 ), ( X3, Y1 ), ( X3, Y3 ), ( X1, Y3 ) ]
            )


{-| All the midpoints of the board.
-}
middles : List Location
middles =
    allLocations
        |> List.filter
            (\( x, y, z ) ->
                member ( x, y ) [ ( X2, Y1 ), ( X1, Y2 ), ( X2, Y3 ), ( X3, Y2 ) ]
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription Animate [ model.style ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( { board = List.repeat 24 Empty
      , moveInProgress = NoClick
      , counter = 0
      , style = initStyle
      }
    , Cmd.none
    )


{-| Initial animation style.
-}
initStyle =
    Animation.style
        [ Animation.attr "cy" 200 "px" ]


{-| Based on the actions of the user updates the model.
-}
update : Msg -> Model -> Model
update msg model =
    let
        currentPlayer =
            model.counter |> whoseTurn

        playerOnPosition location =
            playerOnLocation location model.board

        isCurrentPlayer location =
            playerOnPosition location == currentPlayer

        isOtherPlayer location =
            playerOnPosition location == (whoseTurn <| model.counter + 1)

        isEmptyLocation location =
            playerOnPosition location == Empty

        updateWithPlayer target player =
            { board = updateBoard target player model.board
            , moveInProgress = NoClick
            , counter = model.counter + 1
            , style = model.style
            }

        winnerMove move =
            if isWin <| move then
                winnerAnimation <| move

            else
                move

        selectPiece target =
            { model | moveInProgress = FirstClick target }

        movePiece loc target =
            (if
                model.board
                    |> updateBoard loc Empty
                    |> updateBoard target currentPlayer
                    |> playerLocations currentPlayer
                    |> isMill target
             then
                { board = updateBoard target currentPlayer (updateBoard loc Empty model.board)
                , moveInProgress = SecondClick loc target
                , counter = model.counter
                , style = model.style
                }

             else
                { board = updateBoard target currentPlayer (updateBoard loc Empty model.board)
                , moveInProgress = NoClick
                , counter = model.counter + 1
                , style = model.style
                }
            )
                |> winnerMove

        capturePiece target =
            (if validCapture target model then
                updateWithPlayer target Empty

             else
                model
            )
                |> winnerMove
    in
    case msg of
        Click target ->
            if isWin model then
                model

            else
                case ( phase model, model.moveInProgress ) of
                    ( Place, NoClick ) ->
                        if isEmptyLocation target then
                            if isMill target <| [ target ] ++ playerLocations currentPlayer model.board then
                                { board = updateBoard target currentPlayer model.board
                                , moveInProgress = FirstClick target
                                , counter = model.counter
                                , style = model.style
                                }

                            else
                                updateWithPlayer target currentPlayer

                        else
                            model

                    ( Place, FirstClick loc ) ->
                        capturePiece target

                    ( Move, NoClick ) ->
                        if isCurrentPlayer target then
                            selectPiece target

                        else
                            model

                    ( Move, FirstClick loc ) ->
                        if isCurrentPlayer target then
                            selectPiece target

                        else if allowedMove loc target && isEmptyLocation target then
                            movePiece loc target

                        else
                            model

                    ( Move, SecondClick loc1 loc2 ) ->
                        capturePiece target

                    ( Fly, NoClick ) ->
                        if isCurrentPlayer target then
                            selectPiece target

                        else
                            model

                    ( Fly, FirstClick loc ) ->
                        if isCurrentPlayer target then
                            selectPiece target

                        else if isEmptyLocation target then
                            movePiece loc target

                        else
                            model

                    ( Fly, SecondClick loc1 loc2 ) ->
                        capturePiece target

                    _ ->
                        model

        Animate animMsg ->
            { model
                | style = Animation.update animMsg model.style
            }

        Reset ->
            Tuple.first (init ())


{-| Determine if a player has no legal moves or has < 2 players.
-}
isWin : Model -> Bool
isWin model =
    let
        gT18Counter =
            model.counter > 18

        lT3PiecesFor player =
            length (playerLocations player model.board) < 3

        noValidMovesFor player =
            not
                (playerLocations player model.board
                    |> any
                        (\playerLoc ->
                            playerLocations Empty model.board
                                |> any
                                    (\emptyLoc -> allowedMove playerLoc emptyLoc)
                        )
                )
    in
    gT18Counter && (lT3PiecesFor W || lT3PiecesFor B || noValidMovesFor W || noValidMovesFor B)


{-| The animation visualizing the winner.
-}
winnerAnimation : Model -> Model
winnerAnimation model =
    let
        gT18Counter =
            model.counter > 18

        lT3PiecesFor player =
            length (playerLocations player model.board) < 3

        noValidMovesFor player =
            not
                (playerLocations player model.board
                    |> any
                        (\playerLoc ->
                            playerLocations Empty model.board
                                |> any
                                    (\emptyLoc -> allowedMove playerLoc emptyLoc)
                        )
                )

        loses player =
            gT18Counter && (noValidMovesFor player || lT3PiecesFor player)

        newStyle =
            if loses W then
                Animation.interrupt
                    [ [ Animation.attr "cy" 290 "px" ]
                        |> Animation.toWith
                            ({ duration = 1000, ease = Ease.outElastic }
                                |> Animation.easing
                            )
                    ]
                    model.style

            else if loses B then
                Animation.interrupt
                    [ [ Animation.attr "cy" 110 "px" ]
                        |> Animation.toWith
                            ({ duration = 1000, ease = Ease.outElastic }
                                |> Animation.easing
                            )
                    ]
                    model.style

            else
                model.style
    in
    { model | style = newStyle }


{-| Determine the three phases of the game.
-}
phase : Model -> Phase
phase { counter, board } =
    if counter < 18 then
        Place

    else if 3 < length (playerLocations (whoseTurn counter) board) then
        Move

    else
        Fly


{-| Determine if the location is in a mill
-}
isMill : Location -> List Location -> Bool
isMill location playerLocs =
    allMills
        |> filter (all (\loc -> member loc playerLocs))
        |> any (member location)


{-| Determine if the desired elimination is valid.
-}
validCapture : Location -> Model -> Bool
validCapture loc model =
    let
        otherPlayer =
            whoseTurn <| model.counter + 1

        otherPlayerLocs =
            playerLocations otherPlayer model.board

        isOtherPlayer =
            playerOnLocation loc model.board == otherPlayer

        notInOthersMill =
            not (isMill loc otherPlayerLocs)

        otherPlayerAllMills =
            otherPlayerLocs
                |> all (\playerloc -> isMill playerloc otherPlayerLocs)
    in
    isOtherPlayer && (notInOthersMill || otherPlayerAllMills)


{-| Determine if a move is legal, between two adjacent positions.
-}
allowedMove : Location -> Location -> Bool
allowedMove from to =
    let
        ( ( x1, y1, z1 ), ( x2, y2, z2 ) ) =
            ( from, to )

        isMiddle x =
            member x middles

        onRings1and3 =
            (z1 == Z1 && z2 == Z3) || (z1 == Z3 && z2 == Z1)

        sameXY =
            ( x1, y1 ) == ( x2, y2 )

        sameXZorYZ =
            ( x1, z1 ) == ( x2, z2 ) || ( y1, z1 ) == ( y2, z2 )
    in
    if xor (isMiddle from) (isMiddle to) then
        sameXZorYZ

    else
        (from /= to) && isMiddle from && isMiddle to && sameXY && not onRings1and3


{-| Returns the list of locations where the given player has pieces on the board.
-}
playerLocations : Player -> Board -> List Location
playerLocations player board =
    allLocations
        |> List.filter (\loc -> playerOnLocation loc board == player)


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
updateBoard : Location -> Player -> Board -> Board
updateBoard loc player board =
    setAt (locationIndex loc) player board


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


{-| Based on a number returns a player.
-}
whoseTurn : Int -> Player
whoseTurn x =
    if modBy 2 x == 0 then
        W

    else
        B
