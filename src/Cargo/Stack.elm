module Cargo.Stack exposing (Stacks, Stack, parse)

import Dict exposing (Dict)
import Regex

type alias Stacks = Dict Int Stack
type alias Stack = List Char

parse : List String -> Stacks
parse =
    let
        callParseLine line acc =
            if String.contains "[" line then
                parseLine line acc

            else
                acc

        reverseStacks _ =
            List.reverse

    in
    (List.foldl callParseLine Dict.empty) >> Dict.map reverseStacks

parseLine : String -> Stacks -> Stacks
parseLine line stacks =
    let
        updateStack match previous =
            previous
            |> Maybe.withDefault []
            |> putToStack (String.toList match)

        placeAtRightStack { match } (index, acc) =
            (index + 1, Dict.update index (updateStack match >> Just) acc)

        placeInStacks acc =
            List.foldl placeAtRightStack (1, acc)
    in
    "(\\s{4}|\\[\\w\\])"
    |> Regex.fromString
    |> Maybe.map (\regex -> Regex.find regex line)
    |> Maybe.map (placeInStacks stacks >> Tuple.second)
    |> Maybe.withDefault Dict.empty

putToStack : List Char -> Stack -> Stack
putToStack chars stack =
    case chars of
        '['::char::']'::_ ->
            char::stack

        _ ->
            stack

unwrap : List (Maybe a) -> List a
unwrap items =
    let
        appendIfPresent item acc =
            case item of
                Just value ->
                    value::acc

                _ ->
                    acc
    in
    List.foldl appendIfPresent [] items
