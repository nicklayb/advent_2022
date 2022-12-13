module ElvesPairs exposing (overlappingTasks)

import Maybe

type alias Range = (Int, Int)

type alias Pair = (Range, Range)

type alias Model =
    { pairs : List (Pair)
    }

overlappingTasks : String -> Int
overlappingTasks input =
    { pairs = [] }
    |> parseInput (String.split "\n" input)
    |> (\model -> List.length model.pairs)

parseInput : List String -> Model -> Model
parseInput input model =
    case input of
        [] ->
            model

        line::lines ->
            model
            |> parseLine line
            |> parseInput lines

parseLine : String -> Model -> Model
parseLine line model =
    line
    |> splitTwo ","
    |> Maybe.andThen (mapTuple parseRange)
    |> Maybe.map (putPair model)
    |> Maybe.withDefault model

putPair : Model -> Pair -> Model
putPair model pair =
    if overlapsAtAll pair then
        { model | pairs = pair::model.pairs }
    else
        model

overlapsAtAll : Pair -> Bool
overlapsAtAll (left, right) =
    anyOverlaps left right

anyOverlaps (minLeft, maxLeft) (minRight, maxRight) =
    if minLeft <= minRight then
        maxLeft >= minRight

    else
        maxRight >= minLeft


isOverlapping : Pair -> Bool
isOverlapping (left, right) =
    inBetween left right || inBetween right left

inBetween : Range -> Range -> Bool
inBetween (leftMin, leftMax) (rightMin, rightMax) =
    leftMin >= rightMin && leftMax <= rightMax

parseRange : String -> Maybe Range
parseRange range = 
    range
    |> splitTwo "-"
    |> Maybe.andThen (mapTuple String.toInt)

splitTwo : String -> String -> Maybe (String, String)
splitTwo seperator input = 
    case String.split seperator input of
        [left, right] -> Just (left, right)
        _ -> Nothing

mapTuple : (a -> Maybe b) -> (a, a) -> Maybe (b, b)
mapTuple func (left, right) =
    case (func left, func right) of
        (Just leftMapped, Just rightMapped) -> Just (leftMapped, rightMapped)
        _ -> Nothing

