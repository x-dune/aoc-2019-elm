module Solution.Day4 exposing (metadata, solution)

import Parser exposing ((|.), (|=), Parser, Trailing(..))
import Regex exposing (Regex)
import Solution.Types exposing (PageMetadata, Solution, SolutionOutput(..))
import Util.Helper exposing (metadataHelper)


metadata : PageMetadata
metadata =
    metadataHelper "Day 4: Secure Container" 4


type alias Range =
    ( Int, Int )


rangeParser : Parser Range
rangeParser =
    Parser.succeed Tuple.pair
        |= Parser.int
        |. Parser.symbol "-"
        |= Parser.int


maybePassword : Maybe Regex
maybePassword =
    Regex.fromString "^(?=\\d{6}$)(?=.*(\\d)\\1.*)0*1*2*3*4*5*6*7*8*9*$"


maybeMatchingDigits : Maybe Regex
maybeMatchingDigits =
    Regex.fromString "(\\d)\\1+"


maybeMatchingTwoDigits : Maybe Regex
maybeMatchingTwoDigits =
    Regex.fromString "^(\\d)\\1$"


solution : Solution
solution input =
    let
        range =
            Parser.run rangeParser input

        maybeRegexes =
            Maybe.map3
                (\re1 re2 re3 -> ( re1, re2, re3 ))
                maybePassword
                maybeMatchingDigits
                maybeMatchingTwoDigits
    in
    case ( range, maybeRegexes ) of
        ( Ok ( startRange, endRange ), Just ( password, matchingDigits, matchingTwoDigits ) ) ->
            let
                part1Matches =
                    List.range startRange endRange
                        |> List.map String.fromInt
                        |> List.filter (\item -> Regex.contains password item)

                anyHelper matches =
                    List.any (\{ match } -> Regex.contains matchingTwoDigits match) matches

                part2Matches =
                    part1Matches
                        |> List.map (\item -> Regex.find matchingDigits item)
                        |> List.filter (\item -> anyHelper item)
            in
            Computed
                ( Ok (part1Matches |> List.length |> String.fromInt)
                , Ok (part2Matches |> List.length |> String.fromInt)
                )

        ( Err _, _ ) ->
            Computed ( Err "input parsing error", Err "input parsing error" )

        _ ->
            Computed ( Err "error", Err "error" )
