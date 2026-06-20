module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Dict
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, hidden, id, style)
import Http
import Process
import Random
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
    50


cell : Float
cell =
    (svgSize - edge * 2) / toFloat (boardSize - 1)


stoneRadius : Float
stoneRadius =
    cell * 0.43


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
    svg
        [ SvgAttr.id "board"
        , SvgAttr.viewBox "0 0 1000 1000"
        ]
        [ g [ SvgAttr.id "lines" ] boardLines
        , g [ SvgAttr.id "stars" ] starPoints
        , g [ SvgAttr.id "stones" ] (List.filterMap stoneView moves)
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


stoneView : Move -> Maybe (Svg Msg)
stoneView move =
    move.point
        |> Maybe.map
            (\point ->
                circle
                    [ SvgAttr.class (stoneClass move.color)
                    , SvgAttr.cx (number (boardCoordinate point.x))
                    , SvgAttr.cy (number (boardCoordinate point.y))
                    , SvgAttr.r (number stoneRadius)
                    ]
                    []
            )


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

    else if raw == "B+R" then
        "Black wins by resignation"

    else if raw == "W+R" then
        "White wins by resignation"

    else if String.startsWith "B+" raw then
        "Black wins by " ++ String.dropLeft 2 raw

    else if String.startsWith "W+" raw then
        "White wins by " ++ String.dropLeft 2 raw

    else
        raw


stoneClass : Color -> String
stoneClass color =
    case color of
        Black ->
            "black"

        White ->
            "white"


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
