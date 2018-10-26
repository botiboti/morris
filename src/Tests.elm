module Tests exposing (..)

import List exposing (..)
import List.Extra exposing (getAt)
import Morris exposing (..)


tests : List Bool
tests =
    let
        mFourPieces =
            { board = mkBoard [ ( X2, Y1, Z1 ), ( X3, Y1, Z1 ) ] [ ( X3, Y1, Z3 ), ( X3, Y2, Z1 ) ]
            , moveInProgress = NoClick
            , counter = 4
            , style = (Tuple.first (init ())).style
            }

        mFirstMill =
            { board = mkBoard [ ( X1, Y1, Z1 ), ( X2, Y1, Z1 ), ( X3, Y1, Z1 ) ] [ ( X3, Y1, Z3 ), ( X3, Y2, Z1 ) ]
            , moveInProgress = FirstClick ( X1, Y1, Z1 )
            , counter = 4
            , style = (Tuple.first (init ())).style
            }

        mFirstCapture =
            { board = mkBoard [ ( X1, Y1, Z1 ), ( X2, Y1, Z1 ), ( X3, Y1, Z1 ) ] [ ( X3, Y2, Z1 ) ]
            , moveInProgress = NoClick
            , counter = 5
            , style = (Tuple.first (init ())).style
            }

        mAnOrdinarySituation =
            { board = mkBoard [ ( X3, Y1, Z1 ), ( X3, Y2, Z1 ), ( X3, Y3, Z1 ) ] [ ( X1, Y1, Z3 ), ( X2, Y1, Z3 ), ( X3, Y1, Z3 ) ]
            , moveInProgress = SecondClick ( X2, Y1, Z2 ) ( X2, Y1, Z3 )
            , counter = 53
            , style = (Tuple.first (init ())).style
            }

        mCapturePieceInMill =
            { board = mkBoard [ ( X3, Y1, Z1 ), ( X3, Y2, Z1 ), ( X3, Y3, Z1 ), ( X1, Y3, Z3 ), ( X2, Y3, Z3 ), ( X1, Y2, Z3 ) ] [ ( X3, Y1, Z2 ), ( X3, Y2, Z2 ), ( X3, Y3, Z2 ), ( X3, Y1, Z3 ), ( X3, Y2, Z3 ), ( X3, Y3, Z3 ) ]
            , moveInProgress = FirstClick ( X3, Y1, Z3 )
            , counter = 13
            , style = (Tuple.first (init ())).style
            }

        mLessThan3Piece =
            update (Click ( X3, Y1, Z1 )) mAnOrdinarySituation

        mBlockedPlayer =
            { board = mkBoard [ ( X1, Y1, Z1 ), ( X2, Y1, Z1 ), ( X3, Y1, Z1 ), ( X1, Y2, Z1 ) ] [ ( X3, Y2, Z1 ), ( X2, Y1, Z2 ), ( X1, Y3, Z1 ), ( X1, Y2, Z2 ) ]
            , moveInProgress = NoClick
            , counter = 40
            , style = (Tuple.first (init ())).style
            }

        mJustCapture =
            { board = mkBoard [ ( X1, Y1, Z1 ), ( X2, Y1, Z1 ), ( X3, Y1, Z1 ), ( X1, Y1, Z3 ), ( X2, Y1, Z3 ), ( X3, Y1, Z3 ), ( X3, Y3, Z2 ) ] [ ( X1, Y1, Z2 ), ( X2, Y1, Z2 ), ( X3, Y1, Z2 ), ( X1, Y3, Z3 ), ( X2, Y3, Z3 ), ( X3, Y3, Z3 ), ( X3, Y2, Z2 ) ]
            , moveInProgress = FirstClick ( X3, Y3, Z2 )
            , counter = 18
            , style = (Tuple.first (init ())).style
            }

        mTwoMills =
            { board = mkBoard [ ( X1, Y1, Z1 ), ( X1, Y2, Z1 ), ( X1, Y3, Z1 ) ] [ ( X3, Y1, Z1 ), ( X3, Y2, Z1 ), ( X3, Y3, Z1 ) ]
            , moveInProgress = FirstClick ( X1, Y1, Z1 )
            , counter = 6
            , style = (Tuple.first (init ())).style
            }

        mBlockedPlayer2 =
            { board = mkBoard [ ( X1, Y1, Z1 ), ( X3, Y1, Z1 ), ( X3, Y3, Z1 ), ( X1, Y3, Z1 ), ( X1, Y1, Z3 ), ( X3, Y1, Z3 ), ( X3, Y3, Z3 ), ( X1, Y3, Z3 ) ] [ ( X2, Y1, Z1 ), ( X3, Y2, Z1 ), ( X2, Y3, Z1 ), ( X1, Y2, Z1 ), ( X2, Y1, Z3 ), ( X3, Y2, Z3 ), ( X2, Y3, Z3 ), ( X1, Y2, Z3 ), ( X3, Y2, Z2 ) ]
            , moveInProgress = NoClick
            , counter = 18
            , style = (Tuple.first (init ())).style
            }
    in
    [ -- Completing a mill
      update (Click ( X1, Y1, Z1 )) mFourPieces == mFirstMill

    -- Capturing a piece
    , update (Click ( X3, Y1, Z3 )) mFirstMill == mFirstCapture

    -- Moving from a corner to another corner is not allowed
    , allowedMove ( X1, Y2, Z2 ) ( X3, Y2, Z2 ) == False

    -- Moving two positions vertically is not allowed
    , allowedMove ( X2, Y1, Z3 ) ( X2, Y3, Z3 ) == False

    -- Moving along top radial line is allowed
    , allowedMove ( X2, Y1, Z3 ) ( X2, Y1, Z2 )

    -- Moving from a corner to another corner is not allowed
    , allowedMove ( X1, Y1, Z1 ) ( X3, Y3, Z1 ) == False

    -- Move along left edge on inner ring is allowed
    , allowedMove ( X1, Y2, Z3 ) ( X1, Y1, Z3 )

    -- Moving two positions vertically is not allowed
    , allowedMove ( X3, Y1, Z3 ) ( X1, Y1, Z3 ) == False

    -- Moving a piece on the same location is not allowed
    , allowedMove ( X2, Y1, Z1 ) ( X2, Y1, Z1 ) == False

    -- A valid capture
    , validCapture ( X3, Y1, Z1 ) mAnOrdinarySituation

    -- Capturing a piece in a mill is not allowed, unless there is no other option
    , validCapture ( X3, Y1, Z1 ) mCapturePieceInMill == False

    -- No one has won yet.
    , isWin mAnOrdinarySituation == False

    -- When a player has < 3 pieces is a win condition
    , isWin mLessThan3Piece

    -- When a player has no allowed moves is a win condition
    , isWin mBlockedPlayer

    -- Another blocked player config.
    , isWin mBlockedPlayer2

    -- When a mill is formed the only legal move is to capture a piece.
    , update (Click ( X1, Y3, Z2 )) mJustCapture == mJustCapture

    -- When there are two complete mills a player is not allowed to capture one of his own pieces
    , validCapture ( X1, Y1, Z1 ) mTwoMills == False
    ]


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


{-| Returns the corresponding location for an index.
-}
getLocation : Int -> Location
getLocation i =
    case getAt i allLocations of
        Just a ->
            a

        Nothing ->
            ( X1, Y1, Z1 )
