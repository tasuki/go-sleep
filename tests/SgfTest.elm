module SgfTest exposing (suite)

import Dict
import Expect exposing (Expectation)
import Sgf exposing (Color(..), Game)
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
                        Expect.equal 2 (List.length games)

                    Err message ->
                        Expect.fail message
        , test "reads root properties" <|
            \_ ->
                expectFirstSampleGame <|
                    \game ->
                        Expect.equal (Just [ "001DB3210C08C3CC6FF8D73FBA76456F" ]) (Dict.get "GN" game.properties)
        , test "reads moves with zero-based coordinates" <|
            \_ ->
                expectFirstSampleGame <|
                    \game ->
                        Expect.equal
                            [ { color = Black, point = Just { x = 3, y = 3 }, raw = "dd", comment = Nothing }
                            , { color = White, point = Just { x = 3, y = 15 }, raw = "dp", comment = Nothing }
                            , { color = Black, point = Just { x = 15, y = 15 }, raw = "pp", comment = Nothing }
                            ]
                            (List.take 3 game.moves)
        , test "unescapes property values" <|
            \_ ->
                expectSingleGame "(;C[hello\\] there\\\\ friend];B[])" <|
                    Expect.all
                        [ \game -> Expect.equal (Just [ "hello] there\\ friend" ]) (Dict.get "C" game.properties)
                        , \game -> Expect.equal [ { color = Black, point = Nothing, raw = "", comment = Nothing } ] game.moves
                        ]
        ]


expectFirstSampleGame : (Game -> Expectation) -> Expectation
expectFirstSampleGame assertion =
    case Sgf.parse sample of
        Ok (game :: _) ->
            assertion game

        Ok [] ->
            Expect.fail "expected at least one game"

        Err message ->
            Expect.fail message


expectSingleGame : String -> (Game -> Expectation) -> Expectation
expectSingleGame source assertion =
    case Sgf.parse source of
        Ok [ game ] ->
            assertion game

        Ok games ->
            Expect.fail ("expected one game, got " ++ String.fromInt (List.length games))

        Err message ->
            Expect.fail message
