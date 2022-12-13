module RockPaperScissors exposing (total, totalGuessed)

type Hand = Rock | Paper | Scissors
type Side = Left | Right | Tie

type alias Step =
    { leftHand : Hand
    , rightHand : Hand
    , winningSide : Side
    , expectedOutcome : Side
    , outcomeHand : Hand
    }

type alias Model =
    { steps : List Step }

total : String -> Int
total input = 
    initModel
    |> parseInput (String.split "\n" input)
    |> sumPoints stepToPoint

totalGuessed : String -> Int
totalGuessed input = 
    initModel
    |> parseInput (String.split "\n" input)
    |> sumPoints guessedStepToPoint

initModel : Model
initModel =
    { steps = []}

parseInput : List String -> Model -> Model
parseInput input model =
    case input of
        [] ->
            model

        line::lines ->
            model
            |> parseLine line
            |> parseInput lines

sumPoints : (Step -> Int -> Int) -> Model -> Int
sumPoints func model =
    List.foldl func 0 model.steps

stepToPoint : Step -> Int -> Int
stepToPoint model acc =
    acc + handToPoint model.rightHand + sideToPoint model.winningSide

guessedStepToPoint : Step -> Int -> Int
guessedStepToPoint model acc =
    acc + handToPoint model.outcomeHand + sideToPoint model.expectedOutcome

parseLine : String -> Model -> Model
parseLine line model =
     case String.split " " line of
         [left, right] ->
             { model | steps = (buildStep left right)::model.steps }

         _ ->
            model

buildStep : String -> String -> Step
buildStep left right =
    let
        leftHand = charToHand left
        rightHand = charToHand right
        expectedOutcome = parseExpectedOutcome right
    in
    { leftHand = leftHand
    , rightHand = rightHand
    , winningSide = (winningSide leftHand rightHand)
    , expectedOutcome = expectedOutcome
    , outcomeHand = outcomeHand leftHand expectedOutcome
    }

parseExpectedOutcome : String -> Side
parseExpectedOutcome outcome =
    case outcome of
        "X" -> Left
        "Y" -> Tie
        _ -> Right

outcomeHand : Hand -> Side -> Hand
outcomeHand leftHand expectedOutcome = 
    case expectedOutcome of
        Tie -> leftHand
        Left -> loserAgainst leftHand
        Right -> winnerAgainst leftHand

loserAgainst : Hand -> Hand
loserAgainst hand =
    case hand of
        Rock -> Scissors
        Paper -> Rock
        Scissors -> Paper

winnerAgainst : Hand -> Hand
winnerAgainst hand =
    case hand of
        Rock -> Paper
        Paper -> Scissors
        Scissors -> Rock

winningSide : Hand -> Hand -> Side
winningSide left right = 
    case (left, right) of
        (Rock, Scissors) ->
            Left

        (Paper, Rock) ->
            Left

        (Scissors, Paper) ->
            Left

        (Rock, Rock) ->
            Tie

        (Paper, Paper) ->
            Tie

        (Scissors, Scissors) ->
            Tie

        _ ->
            Right



charToHand : String -> Hand
charToHand char =
    case char of
        "A" -> Rock
        "B" -> Paper
        "C" -> Scissors
        "X" -> Rock
        "Y" -> Paper
        _ -> Scissors


handToPoint : Hand -> Int
handToPoint hand =
    case hand of
        Rock -> 1
        Paper -> 2
        Scissors -> 3

sideToPoint : Side -> Int
sideToPoint side =
    case side of 
        Left -> 0
        Right -> 6
        Tie -> 3
