module Rucksack exposing (itemPriority, commonBadge)

import Debug

type alias Model = 
    { priorities : List (Char, Int)
    }

commonBadge : String -> Int
commonBadge input =
    { priorities = [] }
    |> parseBadges (String.split "\n" input)
    |> .priorities
    |> List.foldl sumPriorities 0

itemPriority : String -> Int
itemPriority input =
    { priorities = [] }
    |> parsePriorities (String.split "\n" input)
    |> .priorities
    |> List.foldl sumPriorities 0

sumPriorities : (Char, Int) -> Int -> Int
sumPriorities (_, priority) acc =
    acc + priority

parseBadges : List String -> Model -> Model
parseBadges input model =
    let
        callFindCommon list rest =
            findCommon model (List.map String.toList list)
            |> parseBadges rest

    in
    case input of
        [] ->
            model

        first::second::third::rest ->
            callFindCommon [first, second, third] rest

        first::second::[] ->
            callFindCommon [first, second] []

        first::[] ->
            callFindCommon [first] []

parsePriorities : List String -> Model -> Model
parsePriorities input model =
    case input of
        [] ->
            model

        line::lines ->
            model
            |> parseLine line
            |> parsePriorities lines

parseLine : String -> Model -> Model
parseLine line model =
    let
        sideLength = (String.length line) // 2
    in
    line
    |> splitAt sideLength
    |> (\(left, right) -> findCommon model [left, right])

findCommon : Model -> List (List Char) -> Model
findCommon model lists =
    case lists of
        [] ->
            model

        (char::chars)::rest ->
            if allIncludes char rest then
                { model | priorities = (char, charPriority char)::model.priorities }

            else
                findCommon model (chars::rest)

        _ ->
            model

allIncludes : Char -> List (List Char) -> Bool
allIncludes char =
    List.all (List.member char)


splitAt : Int -> String -> (List Char, List Char)
splitAt index line =
    let
        accumulate currentIndex leftSide acc =
            case leftSide of
                char::chars ->
                    if currentIndex + 1 == index then
                        (char::acc, chars)

                    else
                        accumulate (currentIndex + 1) chars (char::acc)

                [] ->
                    ([], [])
    in
    accumulate 0 (String.toList line) []

charPriority : Char -> Int
charPriority char =
    if Char.isUpper char then
        Char.toCode char - 38

    else
        Char.toCode char - 96
