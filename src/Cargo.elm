module Cargo exposing (move)

import Dict exposing (Dict)
import Cargo.Parser as Parser
import Cargo.Operation exposing (Operation(..))
import Cargo.Stack exposing (Stacks, Stack)

type alias Model =
    { initialModel : Parser.Model
    , operations : List Operation
    , current : Stacks
    , held : List Char
    }

init : Parser.Model -> Model
init model =
    { initialModel = model, operations = model.operations, current = model.stacks, held = [] }

move : String -> String
move input =
    input
    |> String.split "\n"
    |> Parser.parse
    |> init
    |> applyNext
    |> .current
    |> top

applyNext : Model -> Model
applyNext model =
    case model.operations of
        [] ->
            model

        current::rest ->
            applyNext { model | operations = rest, current = apply current model.current }

apply : Operation -> Stacks -> Stacks
apply (Move amount (from, to)) stacks =
    let 
        (held, rest) =
            take (from, amount) stacks

        putAt toIndex currentHeld updatedStacks =
            Dict.update toIndex (updateStack currentHeld) updatedStacks

        updateStack currentHeld stack =
            case stack of
                Just currentStack ->
                    Just (put currentHeld currentStack)

                Nothing ->
                    Just (put currentHeld [])

    in
    (Move amount (from, to))
    |> (\_ -> rest)
    |> putAt to held


take : (Int, Int) -> Stacks -> (Stack, Stacks)
take (index, amount) stacks = 
    let
        takeAndRemove currentIndex currentStack (crates, acc) =
            let 
                (held, restStack) =
                    popX amount currentStack
            in
            if index == currentIndex then
                (held, Dict.insert currentIndex restStack acc)

            else
                (crates, acc)

    in
    Dict.foldl takeAndRemove ([], stacks) stacks

popX : Int -> Stack -> (Stack, Stack)
popX amount stack =
    let
        popAccumulated currentAmount popped acc =
            case acc of 
                [] ->
                    (popped, acc) 

                (head::rest) ->
                    case currentAmount of 
                        0 ->
                            (popped, head::rest)

                        _ ->
                            popAccumulated (currentAmount - 1) (head::popped) rest

    in
    popAccumulated amount [] stack

put : Stack -> Stack -> Stack
put held stack =
    case held of
        [] ->
            stack

        head::rest ->
            put rest (head::stack)

top : Stacks -> String
top stacks =
    let
        topChar _ stack acc =
            case stack of 
               (head::_) -> head::acc
               _ -> acc
    in
    stacks
    |> Dict.foldl topChar []
    |> List.reverse
    |> String.fromList
