module SgfTest exposing (suite)

import Dict
import Expect
import Sgf exposing (Color(..))
import Test exposing (Test, describe, test)


sample : String
sample =
    "(;FF[4]GM[1]GN[001DB3210C08C3CC6FF8D73FBA76456F]SZ[19]HA[0]KM[6]RE[0];B[dd];W[dp];B[pp])\n(;FF[4]GM[1]GN[00228600D053DD0633D066B58114ABC8]SZ[19]HA[0]KM[6.5]RE[W+R];B[pp];W[dd])"


suite : Test
suite =
    describe "Sgf"
        [ test "reads a collection of SGF game trees" <|
            \_ ->
                case Sgf.parse sample of
                    Ok games ->
                        games
                            |> List.length
                            |> Expect.equal 2

                    Err message ->
                        Expect.fail message
        , test "reads root properties" <|
            \_ ->
                case Sgf.parse sample of
                    Ok (game :: _) ->
                        game.properties
                            |> Dict.get "GN"
                            |> Expect.equal (Just [ "001DB3210C08C3CC6FF8D73FBA76456F" ])

                    Ok [] ->
                        Expect.fail "expected at least one game"

                    Err message ->
                        Expect.fail message
        , test "reads moves with zero-based coordinates" <|
            \_ ->
                case Sgf.parse sample of
                    Ok (game :: _) ->
                        game.moves
                            |> List.take 3
                            |> Expect.equal
                                [ { color = Black, point = Just { x = 3, y = 3 }, raw = "dd" }
                                , { color = White, point = Just { x = 3, y = 15 }, raw = "dp" }
                                , { color = Black, point = Just { x = 15, y = 15 }, raw = "pp" }
                                ]

                    Ok [] ->
                        Expect.fail "expected at least one game"

                    Err message ->
                        Expect.fail message
        , test "unescapes property values" <|
            \_ ->
                case Sgf.parse "(;C[hello\\] there\\\\ friend];B[])" of
                    Ok [ game ] ->
                        Expect.all
                            [ \g -> g.properties |> Dict.get "C" |> Expect.equal (Just [ "hello] there\\ friend" ])
                            , \g -> g.moves |> Expect.equal [ { color = Black, point = Nothing, raw = "" } ]
                            ]
                            game

                    Ok games ->
                        Expect.fail ("expected one game, got " ++ String.fromInt (List.length games))

                    Err message ->
                        Expect.fail message
        ]
