module Tests exposing (..)

import List exposing (..)
import Morris exposing (..)


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

        m4 =
            { board = mkBoard [ ( X3, Y1, Z1 ), ( X3, Y2, Z1 ), ( X3, Y3, Z1 ) ] [ ( X1, Y1, Z3 ), ( X2, Y1, Z3 ), ( X3, Y1, Z3 ) ]
            , moveinprogress = SecondClick ( X2, Y1, Z2 ) ( X2, Y1, Z3 )
            , counter = 53
            }

        m5 =
            { board = mkBoard [ ( X3, Y1, Z1 ), ( X3, Y2, Z1 ), ( X3, Y3, Z1 ), ( X1, Y3, Z3 ), ( X2, Y3, Z3 ), ( X1, Y2, Z3 ) ] [ ( X3, Y1, Z2 ), ( X3, Y2, Z2 ), ( X3, Y3, Z2 ), ( X3, Y1, Z3 ), ( X3, Y2, Z3 ), ( X3, Y3, Z3 ) ]
            , moveinprogress = FirstClick ( X3, Y1, Z3 )
            , counter = 13
            }

        m6 =
            update (Click ( X3, Y1, Z1 )) m4

        m7 =
            { board = mkBoard [ ( X1, Y1, Z1 ), ( X2, Y1, Z1 ), ( X3, Y1, Z1 ), ( X1, Y2, Z1 ) ] [ ( X3, Y2, Z1 ), ( X2, Y1, Z2 ), ( X1, Y3, Z1 ), ( X1, Y2, Z2 ) ]
            , moveinprogress = NoClick
            , counter = 40
            }

        m8 =
            { board = mkBoard [ ( X1, Y1, Z1 ), ( X2, Y1, Z1 ), ( X3, Y1, Z1 ), ( X1, Y1, Z3 ), ( X2, Y1, Z3 ), ( X3, Y1, Z3 ), ( X3, Y3, Z2 ) ] [ ( X1, Y1, Z2 ), ( X2, Y1, Z2 ), ( X3, Y1, Z2 ), ( X1, Y3, Z3 ), ( X2, Y3, Z3 ), ( X3, Y3, Z3 ), ( X3, Y2, Z2 ) ]
            , moveinprogress = FirstClick ( X3, Y3, Z2 )
            , counter = 18
            }
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
    , allowedMove ( X1, Y2, Z2 ) ( X3, Y2, Z2 ) == False
    , allowedMove ( X2, Y1, Z3 ) ( X2, Y3, Z3 ) == False
    , allowedMove ( X2, Y1, Z3 ) ( X2, Y1, Z2 )
    , allowedMove ( X1, Y1, Z1 ) ( X3, Y3, Z1 ) == False
    , allowedMove ( X1, Y2, Z3 ) ( X1, Y1, Z3 )
    , allowedMove ( X3, Y1, Z3 ) ( X1, Y1, Z3 ) == False
    , isRightElimination ( X3, Y1, Z1 ) m4
    , isRightElimination ( X3, Y1, Z1 ) m5 == False
    , isWin m4 == False
    , isWin m6
    , isWin m7
    , update (Click ( X1, Y3, Z2 )) m8 == m8
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
