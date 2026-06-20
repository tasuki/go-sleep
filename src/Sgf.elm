module Sgf exposing
    ( Color(..)
    , Game
    , Move
    , Node
    , Point
    , Property
    , parse
    )

import Char
import Dict exposing (Dict)


type alias Game =
    { properties : Dict String (List String)
    , nodes : List Node
    , moves : List Move
    }


type alias Node =
    List Property


type alias Property =
    { identifier : String
    , values : List String
    }


type alias Move =
    { color : Color
    , point : Maybe Point
    , raw : String
    }


type Color
    = Black
    | White


type alias Point =
    { x : Int
    , y : Int
    }


type alias Parser a =
    String -> Int -> Result String ( a, Int )


parse : String -> Result String (List Game)
parse source =
    parseGames [] source (skipWhitespace source 0)


parseGames : List Game -> String -> Int -> Result String (List Game)
parseGames games source index =
    let
        next =
            skipWhitespace source index
    in
    if next >= String.length source then
        Ok (List.reverse games)

    else if charAt next source == Just '(' then
        case parseGame source next of
            Ok ( game, afterGame ) ->
                parseGames (game :: games) source afterGame

            Err message ->
                Err message

    else
        Err ("expected '(' at offset " ++ String.fromInt next)


parseGame : Parser Game
parseGame source index =
    parseNodes [] source (skipWhitespace source (index + 1))
        |> Result.andThen
            (\( nodes, afterNodes ) ->
                let
                    next =
                        skipWhitespace source afterNodes
                in
                if charAt next source == Just ')' then
                    Ok
                        ( { properties = rootProperties nodes
                          , nodes = nodes
                          , moves = movesFromNodes nodes
                          }
                        , next + 1
                        )

                else
                    Err ("expected ')' at offset " ++ String.fromInt next)
            )


parseNodes : List Node -> Parser (List Node)
parseNodes nodes source index =
    let
        next =
            skipWhitespace source index
    in
    case charAt next source of
        Just ';' ->
            case parseNode source next of
                Ok ( node, afterNode ) ->
                    parseNodes (node :: nodes) source afterNode

                Err message ->
                    Err message

        Just ')' ->
            Ok ( List.reverse nodes, next )

        Just '(' ->
            Err ("variations are not supported yet at offset " ++ String.fromInt next)

        Just _ ->
            Err ("expected ';' or ')' at offset " ++ String.fromInt next)

        Nothing ->
            Err "unexpected end of SGF while reading game"


parseNode : Parser Node
parseNode source index =
    parseProperties [] source (skipWhitespace source (index + 1))


parseProperties : List Property -> Parser Node
parseProperties properties source index =
    let
        next =
            skipWhitespace source index
    in
    case charAt next source of
        Just ';' ->
            Ok ( List.reverse properties, next )

        Just ')' ->
            Ok ( List.reverse properties, next )

        Just '(' ->
            Ok ( List.reverse properties, next )

        Nothing ->
            Ok ( List.reverse properties, next )

        Just c ->
            if Char.isUpper c then
                case parseProperty source next of
                    Ok ( property, afterProperty ) ->
                        parseProperties (property :: properties) source afterProperty

                    Err message ->
                        Err message

            else
                Err ("expected property identifier at offset " ++ String.fromInt next)


parseProperty : Parser Property
parseProperty source index =
    let
        ( identifier, afterIdentifier ) =
            takeWhile Char.isUpper source index
    in
    if String.isEmpty identifier then
        Err ("expected property identifier at offset " ++ String.fromInt index)

    else
        parseValues [] source (skipWhitespace source afterIdentifier)
            |> Result.andThen
                (\( values, afterValues ) ->
                    if List.isEmpty values then
                        Err ("expected value for property " ++ identifier ++ " at offset " ++ String.fromInt afterIdentifier)

                    else
                        Ok ( { identifier = identifier, values = values }, afterValues )
                )


parseValues : List String -> Parser (List String)
parseValues values source index =
    let
        next =
            skipWhitespace source index
    in
    if charAt next source == Just '[' then
        case parseValue source next of
            Ok ( value, afterValue ) ->
                parseValues (value :: values) source afterValue

            Err message ->
                Err message

    else
        Ok ( List.reverse values, next )


parseValue : Parser String
parseValue source index =
    valueLoop [] source (index + 1)


valueLoop : List String -> Parser String
valueLoop chunks source index =
    case charAt index source of
        Nothing ->
            Err "unexpected end of SGF inside property value"

        Just ']' ->
            Ok ( String.concat (List.reverse chunks), index + 1 )

        Just '\\' ->
            case charAt (index + 1) source of
                Nothing ->
                    Err "unexpected end of SGF after escape"

                Just escaped ->
                    valueLoop (String.fromChar escaped :: chunks) source (index + 2)

        Just c ->
            valueLoop (String.fromChar c :: chunks) source (index + 1)


rootProperties : List Node -> Dict String (List String)
rootProperties nodes =
    case nodes of
        root :: _ ->
            root
                |> List.map (\property -> ( property.identifier, property.values ))
                |> Dict.fromList

        [] ->
            Dict.empty


movesFromNodes : List Node -> List Move
movesFromNodes nodes =
    nodes
        |> List.concatMap movesFromNode


movesFromNode : Node -> List Move
movesFromNode node =
    node
        |> List.filterMap moveFromProperty


moveFromProperty : Property -> Maybe Move
moveFromProperty property =
    case ( property.identifier, property.values ) of
        ( "B", raw :: _ ) ->
            Just { color = Black, point = pointFromString raw, raw = raw }

        ( "W", raw :: _ ) ->
            Just { color = White, point = pointFromString raw, raw = raw }

        _ ->
            Nothing


pointFromString : String -> Maybe Point
pointFromString raw =
    case String.toList raw of
        [ xChar, yChar ] ->
            Maybe.map2 Point (coordinate xChar) (coordinate yChar)

        _ ->
            Nothing


coordinate : Char -> Maybe Int
coordinate char =
    let
        code =
            Char.toCode char - Char.toCode 'a'
    in
    if code >= 0 && code < 26 then
        Just code

    else
        Nothing


skipWhitespace : String -> Int -> Int
skipWhitespace source index =
    case charAt index source of
        Just c ->
            if isWhitespace c then
                skipWhitespace source (index + 1)

            else
                index

        Nothing ->
            index


isWhitespace : Char -> Bool
isWhitespace c =
    c == ' ' || c == '\n' || c == '\r' || c == '\t'


takeWhile : (Char -> Bool) -> String -> Int -> ( String, Int )
takeWhile predicate source index =
    takeWhileHelp predicate source index []


takeWhileHelp : (Char -> Bool) -> String -> Int -> List String -> ( String, Int )
takeWhileHelp predicate source index chunks =
    case charAt index source of
        Just c ->
            if predicate c then
                takeWhileHelp predicate source (index + 1) (String.fromChar c :: chunks)

            else
                ( String.concat (List.reverse chunks), index )

        Nothing ->
            ( String.concat (List.reverse chunks), index )


charAt : Int -> String -> Maybe Char
charAt index source =
    source
        |> String.slice index (index + 1)
        |> String.uncons
        |> Maybe.map Tuple.first
