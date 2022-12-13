module Cargo.Utils exposing (splitWhen, mapTuple)

splitWhen : (a -> Bool) -> List a -> (List a, List a)
splitWhen func list =
    let
        split items acc =
            case items of
                [] ->
                    (acc, [])

                head::tail ->
                    if func head then
                        (List.reverse (head::acc), tail)

                    else
                        split tail (head::acc)
    in
    split list []

mapTuple : (a -> Maybe b) -> (a, a, a) -> Maybe (b, b, b)
mapTuple func (left, center, right) =
    rewrap3 (func left, func center, func right)

rewrap3 : (Maybe a, Maybe a, Maybe a) -> Maybe (a, a, a)
rewrap3 tuple =
    case tuple of
        (Just leftMapped, Just centerMapped, Just rightMapped) -> Just (leftMapped, centerMapped, rightMapped)
        _ -> Nothing
