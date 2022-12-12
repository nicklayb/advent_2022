module ElfCalories exposing (run)

type alias Model = 
    { maximum : Int
    , currentStack : List Int
    , elves : List (Int, Int)
    , currentIndex : Int }

run : String -> Int
run input =
    { maximum = 0, currentStack = [], elves = [], currentIndex = 0 }
    |> calculate (String.split "\n" input)
    |> takeThreeTop

takeThreeTop : Model -> Int
takeThreeTop model =
    model.elves
    |> List.map Tuple.second
    |> List.sortWith desc
    |> List.take 3
    |> sum

desc : Int -> Int -> Order
desc left right = 
    case compare left right of
        LT -> GT
        EQ -> EQ
        GT -> LT

calculate : List String -> Model -> Model
calculate lines model =
    case lines of 
        [] ->
            model

        line::remainingLines ->
            case line of
                "" ->
                    model
                    |> store
                    |> calculate remainingLines

                _ ->
                    model
                    |> putLine line
                    |> calculate remainingLines

putLine : String -> Model -> Model
putLine line model =
    let
        lineInt =
            case String.toInt line of
                Just int -> int
                _ -> 0
    in
    { model | currentStack = lineInt::model.currentStack }

store : Model -> Model
store model =
    let
        total = sum model.currentStack
        newModel = 
            { model | currentStack = [], currentIndex = model.currentIndex + 1, elves = (model.currentIndex, total)::model.elves }
        putMaximum maximum =
            { newModel | maximum = maximum }
    in
    if total > model.maximum then
        putMaximum total
    else
        newModel

sum : List Int -> Int
sum =
    List.foldl (+) 0
