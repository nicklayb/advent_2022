module Cargo.Parser exposing (Model, parse)

import Cargo.Operation as Operation exposing (Operation)
import Cargo.Stack as Stack exposing (Stacks)
import Cargo.Utils exposing (splitWhen)

type alias Model =
    { stacks : Stacks
    , operations : List Operation
    }

parseInput : (List String, List String) -> Model
parseInput (stacks, operations) =
    { operations = Operation.parse operations, stacks = Stack.parse stacks }

parse : List String -> Model
parse input = 
    input
    |> splitWhen ((==) "")
    |> parseInput


