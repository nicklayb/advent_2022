module PacketMarker exposing (charsBeforeMarker)

charsBeforeMarker : String -> List Char
charsBeforeMarker input = 
    input
    |> String.toList
    |> charsUntilMarker 14 []

charsUntilMarker : Int -> List Char -> List Char -> List Char
charsUntilMarker expectedLength acc chars =
    case chars of
        [] -> acc

        first::rest -> 
            if anyIntersect (List.take expectedLength chars) then
                charsUntilMarker expectedLength (first::acc) (List.tail chars |> Maybe.withDefault [])

            else
                (List.take expectedLength rest) ++ acc

anyIntersect : List Char -> Bool
anyIntersect list =
    case list of
        head::rest -> 
            if List.member head rest then
                True

            else
                anyIntersect rest

        _ ->
            False

