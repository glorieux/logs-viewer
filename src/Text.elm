module Text exposing (TextResult(..), splitMatchCaseInsensitive)


type TextResult
    = Match String
    | Rest String


extractRecursive : Int -> String -> List Int -> Int -> List TextResult -> List TextResult
extractRecursive separatorLength text indexes currentIndex result =
    case indexes of
        [] ->
            if currentIndex == String.length text then
                List.reverse result

            else
                List.reverse (Rest (String.slice currentIndex (String.length text) text) :: result)

        head :: tail ->
            if head == 0 then
                extractRecursive
                    separatorLength
                    text
                    tail
                    separatorLength
                    (Match (String.slice head (head + separatorLength) text) :: result)

            else
                extractRecursive
                    separatorLength
                    text
                    tail
                    (head + separatorLength)
                    (Match (String.slice head (head + separatorLength) text) :: Rest (String.slice currentIndex head text) :: result)


extract : Int -> String -> List Int -> List TextResult
extract separatorLength text indexes =
    case indexes of
        [] ->
            [ Rest text ]

        _ :: _ ->
            extractRecursive separatorLength text indexes 0 []


splitMatchCaseInsensitive : String -> String -> List TextResult
splitMatchCaseInsensitive separator text =
    let
        separatorLength =
            String.length separator

        indexes =
            String.indexes (String.toLower separator) (String.toLower text)
    in
    extract separatorLength text indexes
