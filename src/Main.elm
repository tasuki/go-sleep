module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Dict
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, hidden, id, style)
import Http
import Process
import Random
import Set exposing (Set)
import Sgf exposing (Color(..), Game, Move)
import Svg exposing (Svg, circle, g, line, svg)
import Svg.Attributes as SvgAttr
import Task
import Url exposing (Url)


stepDelayMilliseconds : Float
stepDelayMilliseconds =
    5 * 1000


boardSize : Int
boardSize =
    19


svgSize : Float
svgSize =
    1000


edge : Float
edge =
    30


cell : Float
cell =
    (svgSize - edge * 2) / toFloat (boardSize - 1)


blackStoneRadius : Float
blackStoneRadius =
    cell * 0.48


whiteStoneRadius : Float
whiteStoneRadius =
    cell * 0.49


type Model
    = Loading
    | Playing Playback
    | Failed String


type alias Playback =
    { game : Game
    , shownMoves : Int
    , showingResult : Bool
    }


type Msg
    = GotSgf (Result Http.Error String)
    | PickedRecord String
    | Step
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url


type alias Board =
    Dict.Dict ( Int, Int ) Color


main : Program () Model Msg
main =
    Browser.application
        { init = \_ _ _ -> init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }


init : ( Model, Cmd Msg )
init =
    ( Loading, fetchSgf )


fetchSgf : Cmd Msg
fetchSgf =
    Http.get
        { url = "public/boring.sgf"
        , expect = Http.expectString GotSgf
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal _ ->
                    ( model, Cmd.none )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged _ ->
            ( model, Cmd.none )

        GotSgf result ->
            case result of
                Ok source ->
                    source
                        |> String.lines
                        |> List.filter (String.isEmpty >> not)
                        |> chooseRecord

                Err error ->
                    ( Failed (httpErrorToString error), Cmd.none )

        PickedRecord record ->
            startGame record

        Step ->
            case model of
                Playing playback ->
                    stepPlayback playback

                _ ->
                    ( model, Cmd.none )


chooseRecord : List String -> ( Model, Cmd Msg )
chooseRecord records =
    case records of
        [] ->
            ( Failed "boring.sgf did not contain any records", Cmd.none )

        first :: rest ->
            ( Loading
            , Random.generate PickedRecord (Random.uniform first rest)
            )


startGame : String -> ( Model, Cmd Msg )
startGame record =
    case Sgf.parse record of
        Ok [ game ] ->
            playGame game

        Ok (game :: _) ->
            playGame game

        Ok [] ->
            ( Failed "randomly selected record did not contain a game", Cmd.none )

        Err message ->
            ( Failed message, Cmd.none )


playGame : Game -> ( Model, Cmd Msg )
playGame game =
    ( Playing
        { game = game
        , shownMoves = initialShownMoves game
        , showingResult = False
        }
    , delay
    )


stepPlayback : Playback -> ( Model, Cmd Msg )
stepPlayback playback =
    let
        moveCount =
            List.length playback.game.moves
    in
    if playback.shownMoves < moveCount then
        ( Playing { playback | shownMoves = playback.shownMoves + 1 }
        , delay
        )

    else if not playback.showingResult then
        ( Playing { playback | showingResult = True }
        , delay
        )

    else
        ( Loading, fetchSgf )


initialShownMoves : Game -> Int
initialShownMoves game =
    if List.isEmpty game.moves then
        0

    else
        1


delay : Cmd Msg
delay =
    Process.sleep stepDelayMilliseconds
        |> Task.perform (\_ -> Step)


view : Model -> Browser.Document Msg
view model =
    { title = "Go Sleep"
    , body =
        [ menu
        , div [ id "board-container" ]
            [ boardView (visibleMoves model)
            , resultView model
            ]
        ]
    }


menu : Html Msg
menu =
    div [ id "menu" ]
        [ div [ class "item" ]
            [ div [ class "icon" ]
                [ text "o"
                , span [ class "tooltip" ] [ text " settings" ]
                ]
            ]
        , div [ class "item" ]
            [ div [ class "icon" ]
                [ text "x"
                , span [ class "tooltip" ] [ text " skip" ]
                ]
            ]
        ]


boardView : List Move -> Html Msg
boardView moves =
    let
        stones =
            moves
                |> boardAfterMoves
                |> Dict.toList
                |> List.map boardStoneView
    in
    svg
        [ SvgAttr.id "board"
        , SvgAttr.viewBox "0 0 1000 1000"
        ]
        [ g [ SvgAttr.id "lines" ] boardLines
        , g [ SvgAttr.id "stars" ] starPoints
        , g [ SvgAttr.id "stones" ] (stones ++ lastMoveMarker moves)
        ]


boardLines : List (Svg Msg)
boardLines =
    List.concatMap linePair (List.range 0 (boardSize - 1))


linePair : Int -> List (Svg Msg)
linePair index =
    let
        position =
            boardCoordinate index
    in
    [ line
        [ SvgAttr.x1 (number edge)
        , SvgAttr.y1 (number position)
        , SvgAttr.x2 (number (svgSize - edge))
        , SvgAttr.y2 (number position)
        ]
        []
    , line
        [ SvgAttr.x1 (number position)
        , SvgAttr.y1 (number edge)
        , SvgAttr.x2 (number position)
        , SvgAttr.y2 (number (svgSize - edge))
        ]
        []
    ]


starPoints : List (Svg Msg)
starPoints =
    [ 3, 9, 15 ]
        |> List.concatMap
            (\x ->
                [ 3, 9, 15 ]
                    |> List.map
                        (\y ->
                            circle
                                [ SvgAttr.cx (number (boardCoordinate x))
                                , SvgAttr.cy (number (boardCoordinate y))
                                , SvgAttr.r "4"
                                ]
                                []
                        )
            )


boardStoneView : ( ( Int, Int ), Color ) -> Svg Msg
boardStoneView ( ( x, y ), color ) =
    circle
        [ SvgAttr.class (stoneClass color)
        , SvgAttr.cx (number (boardCoordinate x))
        , SvgAttr.cy (number (boardCoordinate y))
        , SvgAttr.r (number (stoneRadius color))
        ]
        []


boardAfterMoves : List Move -> Board
boardAfterMoves moves =
    List.foldl applyMove Dict.empty moves


applyMove : Move -> Board -> Board
applyMove move board =
    case move.point of
        Nothing ->
            board

        Just point ->
            let
                position =
                    ( point.x, point.y )

                withStone =
                    Dict.insert position move.color board

                afterCaptures =
                    position
                        |> neighbors
                        |> List.filter (\neighbor -> Dict.get neighbor withStone == Just (oppositeColor move.color))
                        |> List.foldl removeGroupIfCaptured withStone
            in
            removeGroupIfCaptured position afterCaptures


removeGroupIfCaptured : ( Int, Int ) -> Board -> Board
removeGroupIfCaptured position board =
    case Dict.get position board of
        Nothing ->
            board

        Just color ->
            let
                ( group, hasLiberty ) =
                    collectGroup board color [ position ] Set.empty False
            in
            if hasLiberty then
                board

            else
                Set.foldl Dict.remove board group


collectGroup : Board -> Color -> List ( Int, Int ) -> Set ( Int, Int ) -> Bool -> ( Set ( Int, Int ), Bool )
collectGroup board color frontier visited hasLiberty =
    case frontier of
        [] ->
            ( visited, hasLiberty )

        position :: rest ->
            if Set.member position visited then
                collectGroup board color rest visited hasLiberty

            else
                case Dict.get position board of
                    Just stoneColor ->
                        if stoneColor == color then
                            let
                                openNeighbors =
                                    neighbors position
                                        |> List.filter (\neighbor -> Dict.get neighbor board == Nothing)

                                sameColorNeighbors =
                                    neighbors position
                                        |> List.filter (\neighbor -> Dict.get neighbor board == Just color)
                            in
                            collectGroup board color (sameColorNeighbors ++ rest) (Set.insert position visited) (hasLiberty || not (List.isEmpty openNeighbors))

                        else
                            collectGroup board color rest visited hasLiberty

                    Nothing ->
                        collectGroup board color rest visited True


neighbors : ( Int, Int ) -> List ( Int, Int )
neighbors ( x, y ) =
    [ ( x - 1, y ), ( x + 1, y ), ( x, y - 1 ), ( x, y + 1 ) ]
        |> List.filter (\( nx, ny ) -> nx >= 0 && nx < boardSize && ny >= 0 && ny < boardSize)


oppositeColor : Color -> Color
oppositeColor color =
    case color of
        Black ->
            White

        White ->
            Black


lastMoveMarker : List Move -> List (Svg Msg)
lastMoveMarker moves =
    case lastMoveWithPoint moves of
        Just move ->
            case move.point of
                Just point ->
                    [ circle
                        [ SvgAttr.class (oppositeStoneClass move.color)
                        , SvgAttr.cx (number (boardCoordinate point.x))
                        , SvgAttr.cy (number (boardCoordinate point.y))
                        , SvgAttr.r (number (cell * 0.15))
                        , SvgAttr.stroke "none"
                        ]
                        []
                    ]

                Nothing ->
                    []

        Nothing ->
            []


lastMoveWithPoint : List Move -> Maybe Move
lastMoveWithPoint moves =
    List.foldl
        (\move previous ->
            case move.point of
                Just _ ->
                    Just move

                Nothing ->
                    previous
        )
        Nothing
        moves


resultView : Model -> Html Msg
resultView model =
    case model of
        Playing playback ->
            div
                [ id "result"
                , hidden (not playback.showingResult)
                ]
                [ text (formatResult playback.game) ]

        Failed message ->
            div [ id "result" ] [ text message ]

        _ ->
            div [ id "result", hidden True ] []


visibleMoves : Model -> List Move
visibleMoves model =
    case model of
        Playing playback ->
            List.take playback.shownMoves playback.game.moves

        _ ->
            []


formatResult : Game -> String
formatResult game =
    let
        raw =
            game.properties
                |> Dict.get "RE"
                |> Maybe.andThen List.head
                |> Maybe.withDefault "0"
    in
    if raw == "0" || raw == "" then
        "Draw"

    else
        raw


stoneClass : Color -> String
stoneClass color =
    case color of
        Black ->
            "black"

        White ->
            "white"


oppositeStoneClass : Color -> String
oppositeStoneClass color =
    case color of
        Black ->
            "white"

        White ->
            "black"


stoneRadius : Color -> Float
stoneRadius color =
    case color of
        Black ->
            blackStoneRadius

        White ->
            whiteStoneRadius


boardCoordinate : Int -> Float
boardCoordinate index =
    edge + toFloat index * cell


number : Float -> String
number value =
    String.fromFloat value


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Timed out loading boring.sgf"

        Http.NetworkError ->
            "Network error loading boring.sgf"

        Http.BadStatus status ->
            "Could not load boring.sgf: HTTP " ++ String.fromInt status

        Http.BadBody message ->
            message
