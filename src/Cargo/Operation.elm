module Cargo.Operation exposing (Transition, Operation(..), parse)

import Cargo.Utils exposing (mapTuple)
import Regex

type alias Transition = (Int, Int)

type Operation = Move Int Transition

parse : List String -> List Operation
parse =
    let
        putValidOperation operation acc =
            case parseOperation operation of
                Just op ->
                    op::acc

                _ ->
                    acc
    in
    (List.foldl putValidOperation []) >> List.reverse

parseOperation : String -> Maybe Operation
parseOperation operation =
    let
        toOperation (amount, from, to) =
            Move amount (from, to)

        mapToOperation matches =
            matches
            |> unwrapMatches
            |> Maybe.andThen (mapTuple String.toInt)
            |> Maybe.map toOperation

        unwrapMatches matches =
            case matches of 
                [Just first, Just second, Just third] ->
                    Just (first, second, third)

                _ ->
                    Nothing
    in
    "move (\\d+) from (\\d+) to (\\d+)"
    |> Regex.fromString
    |> Maybe.map (\regex -> Regex.find regex operation)
    |> Maybe.andThen List.head
    |> Maybe.andThen (.submatches >> mapToOperation)
