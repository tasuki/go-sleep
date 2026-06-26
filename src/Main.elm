port module Main exposing (main)

import Browser
import Browser.Events
import Browser.Navigation as Nav
import Dict
import Html exposing (Html, div, p, input, li, span, strong, text, ul)
import Html.Attributes as HtmlAttr exposing (class, hidden, id, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Random
import Set exposing (Set)
import Sgf exposing (Color(..), Game, Move)
import Svg exposing (Svg, circle, g, line, svg)
import Svg.Attributes as SvgAttr
import Time
import Url exposing (Url)



resultSeconds : Int
resultSeconds =
    10


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
    = Loading Settings
    | Playing Playback
    | Failed Settings String


type alias Settings =
    { collection : Collection
    , replaySeconds : Int
    , theme : Theme
    , bar : Bar
    }


type Collection
    = Boring
    | Exciting


type Theme
    = Night
    | Day


type Bar
    = HideBar
    | ShowBar


type alias Playback =
    { game : Game
    , board : Board
    , remainingMoves : List Move
    , lastMove : Maybe Move
    , settings : Settings
    , settingsOpen : Bool
    , helpOpen : Bool
    , showingResult : Bool
    }


type Msg
    = GotSgf (Result Http.Error String)
    | PickedRecord String
    | ToggleSettings
    | CloseSettings
    | ToggleHelp
    | CloseHelp
    | SetCollection String
    | SetReplaySeconds String
    | SetTheme String
    | SetBar String
    | Step
    | Skip
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url


type alias Board =
    Dict.Dict ( Int, Int ) Color


port saveCollection : String -> Cmd msg


port saveReplaySeconds : Int -> Cmd msg


port saveTheme : String -> Cmd msg


port saveBar : String -> Cmd msg


type alias Flags =
    { collection : String
    , replaySeconds : Int
    , theme : String
    , bar : String
    }


main : Program Flags Model Msg
main =
    Browser.application
        { init = \replaySeconds _ _ -> init replaySeconds
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        settings =
            settingsFromFlags flags
    in
    ( Loading settings, fetchSgf settings )


fetchSgf : Settings -> Cmd Msg
fetchSgf settings =
    Http.get
        { url = collectionUrl settings.collection
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
                        |> chooseRecord (settingsOf model)

                Err error ->
                    ( Failed (settingsOf model) (httpErrorToString error), Cmd.none )

        PickedRecord record ->
            startGame (settingsOf model) record

        ToggleSettings ->
            case model of
                Playing playback ->
                    ( Playing { playback | settingsOpen = not playback.settingsOpen, helpOpen = False }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        CloseSettings ->
            case model of
                Playing playback ->
                    ( Playing { playback | settingsOpen = False }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ToggleHelp ->
            case model of
                Playing playback ->
                    ( Playing { playback | helpOpen = not playback.helpOpen, settingsOpen = False }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        CloseHelp ->
            case model of
                Playing playback ->
                    ( Playing { playback | helpOpen = False }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SetCollection rawCollection ->
            case model of
                Playing playback ->
                    let
                        settings =
                            playback.settings

                        collection =
                            collectionFromString rawCollection

                        newSettings =
                            { settings | collection = collection }
                    in
                    ( Loading newSettings
                    , Cmd.batch
                        [ saveCollection (collectionToString collection)
                        , fetchSgf newSettings
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        SetReplaySeconds rawSeconds ->
            case ( model, String.toInt rawSeconds ) of
                ( Playing playback, Just seconds ) ->
                    let
                        settings =
                            playback.settings

                        replaySeconds =
                            validReplaySeconds seconds
                    in
                    ( Playing { playback | settings = { settings | replaySeconds = replaySeconds } }
                    , saveReplaySeconds replaySeconds
                    )

                _ ->
                    ( model, Cmd.none )

        SetTheme rawTheme ->
            case model of
                Playing playback ->
                    let
                        settings =
                            playback.settings

                        theme =
                            themeFromString rawTheme
                    in
                    ( Playing { playback | settings = { settings | theme = theme } }
                    , saveTheme (themeToString theme)
                    )

                _ ->
                    ( model, Cmd.none )

        SetBar rawBar ->
            case model of
                Playing playback ->
                    let
                        settings =
                            playback.settings

                        bar =
                            barFromString rawBar
                    in
                    ( Playing { playback | settings = { settings | bar = bar } }
                    , saveBar (barToString bar)
                    )

                _ ->
                    ( model, Cmd.none )

        Step ->
            case model of
                Playing playback ->
                    stepPlayback playback

                _ ->
                    ( model, Cmd.none )

        Skip ->
            let
                settings =
                    settingsOf model
            in
            ( Loading settings, fetchSgf settings )


chooseRecord : Settings -> List String -> ( Model, Cmd Msg )
chooseRecord settings records =
    case records of
        [] ->
            ( Failed settings (collectionFileName settings.collection ++ " did not contain any records"), Cmd.none )

        first :: rest ->
            ( Loading settings
            , Random.generate PickedRecord (Random.uniform first rest)
            )


startGame : Settings -> String -> ( Model, Cmd Msg )
startGame settings record =
    case Sgf.parse record of
        Ok [ game ] ->
            playGame settings game

        Ok (game :: _) ->
            playGame settings game

        Ok [] ->
            ( Failed settings "randomly selected record did not contain a game", Cmd.none )

        Err message ->
            ( Failed settings message, Cmd.none )


playGame : Settings -> Game -> ( Model, Cmd Msg )
playGame settings game =
    ( Playing (initialPlayback settings game)
    , Cmd.none
    )


initialPlayback : Settings -> Game -> Playback
initialPlayback settings game =
    let
        ( board, remainingMoves, lastMove ) =
            case game.moves of
                firstMove :: rest ->
                    ( applyMove firstMove Dict.empty, rest, rememberLastMove firstMove Nothing )

                [] ->
                    ( Dict.empty, [], Nothing )
    in
    { game = game
    , board = board
    , remainingMoves = remainingMoves
    , lastMove = lastMove
    , settings = settings
    , settingsOpen = False
    , helpOpen = False
    , showingResult = False
    }


stepPlayback : Playback -> ( Model, Cmd Msg )
stepPlayback playback =
    case playback.remainingMoves of
        move :: rest ->
            ( Playing
                { playback
                    | board = applyMove move playback.board
                    , remainingMoves = rest
                    , lastMove = rememberLastMove move playback.lastMove
                }
            , Cmd.none
            )

        [] ->
            if not playback.showingResult then
                ( Playing { playback | showingResult = True }
                , Cmd.none
                )

            else
                ( Loading playback.settings, fetchSgf playback.settings )


rememberLastMove : Move -> Maybe Move -> Maybe Move
rememberLastMove move previous =
    case move.point of
        Just _ ->
            Just move

        Nothing ->
            previous


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown keyDecoder
        , playbackSubscription model
        ]


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                if key == "?" then
                    Decode.succeed ToggleHelp

                else
                    Decode.fail "ignored key"
            )


playbackSubscription : Model -> Sub Msg
playbackSubscription model =
    case model of
        Playing playback ->
            if playback.settingsOpen || playback.helpOpen then
                Sub.none

            else if playback.showingResult then
                Time.every (toFloat resultSeconds * 1000) (\_ -> Step)

            else
                Time.every (toFloat playback.settings.replaySeconds * 1000) (\_ -> Step)

        _ ->
            Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "Go Sleep"
    , body =
        [ div [ id "app", class (appClass model) ]
            [ menu
            , div [ id "board-container" ]
                [ div [ id "board-stack" ]
                    [ boardView (visibleBoard model) (visibleLastMove model)
                    , winProbabilityBarView (settingsOf model) (visibleLastMove model)
                    ]
                , settingsView model
                , helpView model
                , resultView model
                ]
            ]
        ]
    }


menu : Html Msg
menu =
    div [ id "menu" ]
        [ div [ class "item" ]
            [ div [ class "icon", onClick ToggleSettings ]
                [ text "o"
                , span [ class "tooltip" ] [ text " settings" ]
                ]
            ]
        , div [ class "item" ]
            [ div [ class "icon", onClick ToggleHelp ]
                [ text "?"
                , span [ class "tooltip" ] [ text " help" ]
                ]
            ]
        , div [ class "item" ]
            [ div [ class "icon", onClick Skip ]
                [ text "x"
                , span [ class "tooltip" ] [ text " skip" ]
                ]
            ]
        ]


settingsView : Model -> Html Msg
settingsView model =
    case model of
        Playing playback ->
            div []
                [ div
                    [ id "close-settings-area"
                    , hidden (not playback.settingsOpen)
                    , onClick CloseSettings
                    ]
                    []
                , div
                    [ id "settings-overlay"
                    , hidden (not playback.settingsOpen)
                    ]
                    [ div [ class "outside-board", onClick CloseSettings ] []
                    , div [ class "settings-board" ]
                        [ div [ class "setting" ]
                            [ div [ class "setting-row" ]
                                [ span [ class "setting-label" ] [ text "Game" ]
                                , span [] [ text (gameName playback.game) ]
                                ]
                            , div [ class "setting-row" ]
                                [ span [ class "setting-label" ] [ text "Speed" ]
                                , span [] [ text (String.fromInt playback.settings.replaySeconds ++ " seconds per move") ]
                                , input
                                    [ type_ "range"
                                    , HtmlAttr.min "1"
                                    , HtmlAttr.max "10"
                                    , HtmlAttr.step "1"
                                    , value (String.fromInt playback.settings.replaySeconds)
                                    , onInput SetReplaySeconds
                                    ]
                                    []
                                ]
                            , div [ class "setting-row" ]
                                [ span [ class "setting-label" ] [ text "Collection" ]
                                , div [ class "collections" ]
                                    [ span
                                        [ class (optionClass (playback.settings.collection == Boring))
                                        , onClick (SetCollection "boring")
                                        ]
                                        [ text "boring" ]
                                    , span
                                        [ class (optionClass (playback.settings.collection == Exciting))
                                        , onClick (SetCollection "exciting")
                                        ]
                                        [ text "exciting" ]
                                    ]
                                ]
                            , div [ class "setting-row" ]
                                [ span [ class "setting-label" ] [ text "Theme" ]
                                , div [ class "themes" ]
                                    [ span
                                        [ class (optionClass (playback.settings.theme == Night))
                                        , onClick (SetTheme "night")
                                        ]
                                        [ text "night" ]
                                    , span
                                        [ class (optionClass (playback.settings.theme == Day))
                                        , onClick (SetTheme "day")
                                        ]
                                        [ text "day" ]
                                    ]
                                ]
                            , div [ class "setting-row" ]
                                [ span [ class "setting-label" ] [ text "Win bar" ]
                                , div [ class "bar-options" ]
                                    [ span
                                        [ class (optionClass (playback.settings.bar == HideBar))
                                        , onClick (SetBar "hide")
                                        ]
                                        [ text "hide" ]
                                    , span
                                        [ class (optionClass (playback.settings.bar == ShowBar))
                                        , onClick (SetBar "show")
                                        ]
                                        [ text "show" ]
                                    ]
                                ]
                            ]
                        ]
                    , div [ class "outside-board", onClick CloseSettings ] []
                    ]
                ]

        _ ->
            div [] []


helpView : Model -> Html Msg
helpView model =
    case model of
        Playing playback ->
            div []
                [ div
                    [ id "close-help-area"
                    , hidden (not playback.helpOpen)
                    , onClick CloseHelp
                    ]
                    []
                , div
                    [ id "help-overlay"
                    , hidden (not playback.helpOpen)
                    ]
                    [ div [ class "outside-board", onClick CloseHelp ] []
                    , div [ class "help-board" ]
                        [ div [ class "help" ]
                            [ p [] [ text "Collections of kata1-b28c512nbt games from 2025:" ]
                            , ul []
                                [ li []
                                    [ strong [] [ text "boring: " ]
                                    , text "Games where win percentage stays between 40% and 60% till move 150. Records are cut to the first 150 moves."
                                    ]
                                , li []
                                    [ strong [] [ text "exciting: " ]
                                    , text "Games ending in fewer than 100 moves."
                                    ]
                                ]
                            ]
                        ]
                    , div [ class "outside-board", onClick CloseHelp ] []
                    ]
                ]

        _ ->
            div [] []


boardView : Board -> Maybe Move -> Html Msg
boardView board lastMove =
    svg
        [ SvgAttr.id "board"
        , SvgAttr.viewBox "0 0 1000 1000"
        ]
        [ g [ SvgAttr.id "lines" ] boardLines
        , g [ SvgAttr.id "stars" ] starPoints
        , g [ SvgAttr.id "stones" ]
            (List.map boardStoneView (Dict.toList board) ++ lastMoveMarker lastMove)
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
                                neighborPositions =
                                    neighbors position

                                hasOpenNeighbor =
                                    List.any (\neighbor -> Dict.get neighbor board == Nothing) neighborPositions

                                sameColorNeighbors =
                                    List.filter (\neighbor -> Dict.get neighbor board == Just color) neighborPositions
                            in
                            collectGroup board color (sameColorNeighbors ++ rest) (Set.insert position visited) (hasLiberty || hasOpenNeighbor)

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


lastMoveMarker : Maybe Move -> List (Svg Msg)
lastMoveMarker lastMove =
    case lastMove |> Maybe.andThen (\move -> Maybe.map (Tuple.pair move) move.point) of
        Just ( move, point ) ->
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


type alias Analysis =
    { blackWin : Float
    , whiteWin : Float
    , score : Int
    }


winProbabilityBarView : Settings -> Maybe Move -> Html Msg
winProbabilityBarView settings maybeMove =
    case visibleAnalysis settings maybeMove of
        Just analysis ->
            let
                white =
                    clamp 0 100 analysis.whiteWin

                black =
                    clamp 0 100 analysis.blackWin

                middle =
                    clamp 0 100 (100 - white - black)
            in
            div [ id "win-probability-bar" ]
                [ div
                    [ class "white-win-probability"
                    , HtmlAttr.style "width" (percent white)
                    ]
                    []
                , div
                    [ class "middle-probability"
                    , HtmlAttr.style "width" (percent middle)
                    ]
                    []
                , div
                    [ class "black-win-probability"
                    , HtmlAttr.style "width" (percent black)
                    ]
                    []
                ]

        Nothing ->
            text ""


visibleAnalysis : Settings -> Maybe Move -> Maybe Analysis
visibleAnalysis settings maybeMove =
    if settings.bar == ShowBar then
        maybeMove |> Maybe.andThen .comment |> Maybe.andThen analysisFromComment

    else
        Nothing


appClass : Model -> String
appClass model =
    let
        settings =
            settingsOf model

        barClass =
            case model of
                Playing playback ->
                    if visibleAnalysis settings playback.lastMove /= Nothing then
                        " bar-visible"

                    else
                        ""

                _ ->
                    ""
    in
    "theme-" ++ themeToString settings.theme ++ barClass


analysisFromComment : String -> Maybe Analysis
analysisFromComment comment =
    case String.words comment of
        whiteRaw :: blackRaw :: scoreRaw :: _ ->
            Maybe.map3 analysisFromCompactNumbers
                (String.toFloat whiteRaw)
                (String.toFloat blackRaw)
                (String.toFloat scoreRaw)

        _ ->
            Nothing


analysisFromCompactNumbers : Float -> Float -> Float -> Analysis
analysisFromCompactNumbers white black score =
    { whiteWin = white
    , blackWin = black
    , score = roundedInt score
    }


roundedInt : Float -> Int
roundedInt value =
    if value >= 0 then
        floor (value + 0.5)

    else
        ceiling (value - 0.5)


percent : Float -> String
percent value =
    String.fromFloat value ++ "%"


resultView : Model -> Html Msg
resultView model =
    case model of
        Playing playback ->
            div
                [ id "result"
                , hidden (not playback.showingResult)
                ]
                [ text (formatResult playback.game) ]

        Failed _ message ->
            div [ id "result" ] [ text message ]

        _ ->
            div [ id "result", hidden True ] []


visibleBoard : Model -> Board
visibleBoard model =
    case model of
        Playing playback ->
            playback.board

        _ ->
            Dict.empty


visibleLastMove : Model -> Maybe Move
visibleLastMove model =
    case model of
        Playing playback ->
            playback.lastMove

        _ ->
            Nothing


settingsOf : Model -> Settings
settingsOf model =
    case model of
        Loading settings ->
            settings

        Playing playback ->
            playback.settings

        Failed settings _ ->
            settings


settingsFromFlags : Flags -> Settings
settingsFromFlags flags =
    { collection = collectionFromString flags.collection
    , replaySeconds = validReplaySeconds flags.replaySeconds
    , theme = themeFromString flags.theme
    , bar = barFromString flags.bar
    }


validReplaySeconds : Int -> Int
validReplaySeconds replaySeconds =
    clamp 1 10 replaySeconds


collectionFromString : String -> Collection
collectionFromString rawCollection =
    case rawCollection of
        "exciting" ->
            Exciting

        _ ->
            Boring


collectionToString : Collection -> String
collectionToString collection =
    case collection of
        Boring ->
            "boring"

        Exciting ->
            "exciting"


collectionFileName : Collection -> String
collectionFileName collection =
    collectionToString collection ++ ".sgf"


collectionUrl : Collection -> String
collectionUrl collection =
    "public/" ++ collectionFileName collection


themeFromString : String -> Theme
themeFromString rawTheme =
    case rawTheme of
        "day" ->
            Day

        _ ->
            Night


themeToString : Theme -> String
themeToString theme =
    case theme of
        Night ->
            "night"

        Day ->
            "day"


barFromString : String -> Bar
barFromString rawBar =
    case rawBar of
        "show" ->
            ShowBar

        _ ->
            HideBar


barToString : Bar -> String
barToString bar =
    case bar of
        HideBar ->
            "hide"

        ShowBar ->
            "show"


gameName : Game -> String
gameName game =
    game.properties
        |> Dict.get "GN"
        |> Maybe.andThen List.head
        |> Maybe.withDefault "Untitled game"


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


optionClass : Bool -> String
optionClass active =
    if active then
        "option active"

    else
        "option"


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
            "Timed out loading SGF"

        Http.NetworkError ->
            "Network error loading SGF"

        Http.BadStatus status ->
            "Could not load SGF: HTTP " ++ String.fromInt status

        Http.BadBody message ->
            message
