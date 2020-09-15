module Table exposing (Problem(..), Row, listToRow, parse, rowToList, rowToMatrix, singleton, stringToRow, testRow)

import Parser as P exposing ((|.), (|=))


type Row
    = Nothing
    | Just String Row
    | Up String Row
    | Down String Row
    | Undefined Int String Row


testRow : Row
testRow =
    Just "x" (Just "0" (Up "len" (Just "1" (Down "xuong" (Just "0" Nothing)))))


type Position
    = Only Int
    | All


rowToList : Int -> Row -> List ( String, Position )
rowToList start row =
    case row of
        Nothing ->
            []

        Just value tail ->
            ( value, Only start ) :: rowToList start tail

        Up value tail ->
            ( value, Only (start + 1) ) :: rowToList (start + 2) tail

        Down value tail ->
            ( value, Only (start - 1) ) :: rowToList (start - 2) tail

        Undefined lvl value tail ->
            ( value, All ) :: rowToList (start + lvl) tail


listToRow : List String -> Row
listToRow list =
    case list of
        [] ->
            Nothing

        head :: tail ->
            case head of
                "lên" ->
                    Up "\\nearrow" (listToRow tail)

                "xuống" ->
                    Down "\\searrow" (listToRow tail)

                "." ->
                    Just "" (listToRow tail)

                "-vc" ->
                    Just "-\\infty" (listToRow tail)

                "+vc" ->
                    Just "+\\infty" (listToRow tail)

                head_ ->
                    case String.left 2 head_ of
                        "||" ->
                            case String.toInt (String.dropLeft 2 head_) of
                                Maybe.Just lvl ->
                                    Undefined lvl "||" (listToRow tail)
                                
                                Maybe.Nothing ->
                                    Undefined 0 "||" (listToRow tail)

                        _ ->
                            Just head (listToRow tail)


stringToRow : String -> Row
stringToRow string =
    string |> String.words |> listToRow


rowToMatrix : Row -> List (List String)
rowToMatrix row =
    let
        positionList =
            rowToList 0 row

        levels =
            positionList
                |> List.map
                    (Tuple.second
                        >> (\lvl ->
                                case lvl of
                                    Only lvl_ ->
                                        lvl_

                                    All ->
                                        0
                           )
                    )

        highestLevel =
            List.maximum levels |> Maybe.withDefault 0

        lowestLevel =
            List.minimum levels |> Maybe.withDefault 0

        itemsOnLevel : List ( String, Position ) -> Int -> List String
        itemsOnLevel items level =
            List.map
                (\( label, lvl ) ->
                    case lvl of
                        Only lvl_ ->
                            if lvl_ == level then
                                " " ++ label ++ " "

                            else
                                String.repeat (String.length label + 2) " "

                        All ->
                            " || "
                )
                items
    in
    List.range lowestLevel highestLevel
        |> List.map (itemsOnLevel positionList)
        |> List.reverse


type Problem
    = ExpectingBracket Int Int
    | BadString String
    | StuckAt Int Int


parse : String -> Result Problem Row
parse text =
    case P.run rowParser text of
        Ok row ->
            Ok row

        Err (error :: _) ->
            case error.problem of
                P.ExpectingSymbol ")" ->
                    Err (ExpectingBracket error.col error.row)

                P.Problem prob ->
                    Err (BadString prob)

                _ ->
                    Err (StuckAt error.col error.row)

        Err [] ->
            Err (StuckAt 0 0)


rowParser : P.Parser Row
rowParser =
    P.succeed listToRow
        |= P.loop [] step


step : List String -> P.Parser (P.Step (List String) (List String))
step list =
    let
        finalize entry_ next =
            next (String.trim entry_ :: list)
    in
    P.succeed finalize
        |= entry
        |= P.oneOf
            [ P.succeed (P.Done << List.reverse)
                |. P.end
            , P.succeed P.Loop
            ]


entry : P.Parser String
entry =
    P.succeed identity
        |. P.spaces
        |= P.oneOf
            [ P.succeed identity
                |. P.symbol "("
                |= P.map (String.dropRight 1) groupParser
            , P.chompWhile (\c -> c /= ' ')
                |. P.spaces
                |> P.getChompedString
            ]
        |. P.spaces


groupParser : P.Parser String
groupParser =
    P.succeed (++)
        |. P.spaces
        |= P.getChompedString (P.chompWhile (\c -> c /= '(' && c /= ')'))
        |= P.oneOf
            [ P.succeed (++)
                |= P.getChompedString (P.symbol "(")
                |= (P.succeed (++)
                        |= P.lazy (\_ -> groupParser)
                        |= P.lazy (\_ -> groupParser)
                   )
            , P.succeed identity
                |= P.getChompedString (P.symbol ")")
            ]


singleton : String -> Row
singleton string =
    Just string Nothing
