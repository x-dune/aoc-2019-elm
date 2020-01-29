module Solution.Day3 exposing (metadata, solution)

import Parser exposing ((|.), (|=), Parser, Trailing(..))
import Set
import Solution.Types exposing (PageMetadata, Solution, SolutionOutput(..))


metadata : PageMetadata
metadata =
    { title = "Day 3: Crossed Wires"
    , sourceCodeLink = "https://github.com/adamdune/aoc-2019-elm/blob/master/src/Solution/Day3.elm"
    , aocPuzzleLink = "https://adventofcode.com/2019/day/3"
    }


type alias Coord =
    ( Int, Int )


type alias Path =
    ( Direction, Int )


type Direction
    = Up
    | Down
    | Left
    | Right


directionParser : Parser Direction
directionParser =
    Parser.oneOf
        [ Parser.map (\_ -> Up) (Parser.symbol "U")
        , Parser.map (\_ -> Down) (Parser.symbol "D")
        , Parser.map (\_ -> Left) (Parser.symbol "L")
        , Parser.map (\_ -> Right) (Parser.symbol "R")
        ]


pathParser : Parser Path
pathParser =
    Parser.succeed Tuple.pair
        |= directionParser
        |= Parser.int


wireParser : Parser (List Path)
wireParser =
    Parser.sequence
        { start = ""
        , separator = ","
        , end = ""
        , spaces = Parser.spaces
        , item = pathParser
        , trailing = Optional
        }


inputParser : Parser ( List Path, List Path )
inputParser =
    Parser.succeed Tuple.pair
        |= wireParser
        |. Parser.spaces
        |= wireParser


travelToPaths : List Path -> Coord -> List Coord
travelToPaths paths start =
    case paths of
        [] ->
            []

        path :: pathsTail ->
            let
                travelled =
                    travelToPath path start

                lastCoord =
                    travelled |> List.reverse |> List.head |> Maybe.withDefault start
            in
            travelled ++ travelToPaths pathsTail lastCoord


travelToPath : Path -> Coord -> List Coord
travelToPath ( direction, magnitude ) ( currX, currY ) =
    if magnitude == 0 then
        []

    else
        case direction of
            Up ->
                List.range (currY + 1) (currY + magnitude)
                    |> List.map (\y -> ( currX, y ))

            Down ->
                List.range (currY - magnitude) (currY - 1)
                    |> List.reverse
                    |> List.map (\y -> ( currX, y ))

            Left ->
                List.range (currX - magnitude) (currX - 1)
                    |> List.reverse
                    |> List.map (\x -> ( x, currY ))

            Right ->
                List.range (currX + 1) (currX + magnitude)
                    |> List.map (\x -> ( x, currY ))


getIntersections : List Coord -> List Coord -> List Coord
getIntersections wire1 wire2 =
    Set.intersect (Set.fromList wire1) (Set.fromList wire2)
        |> Set.toList


distanceFromOrigin : Coord -> Int
distanceFromOrigin ( x, y ) =
    abs x + abs y


distanceOfClosestIntersection : List Coord -> Maybe Int
distanceOfClosestIntersection intersections =
    List.filter (\item -> item /= ( 0, 0 )) intersections
        |> List.map distanceFromOrigin
        |> List.minimum


mapWithSteps : List Coord -> List ( Int, Coord )
mapWithSteps travelled =
    List.indexedMap (\index item -> ( index + 1, item )) travelled


filterSteps : List Coord -> List ( Int, Coord ) -> List ( Int, Coord )
filterSteps intersections steps =
    List.filter (\item -> List.member (Tuple.second item) intersections) steps


distanceOfSmallestStepIntersection : List Coord -> List Coord -> List Coord -> Maybe Int
distanceOfSmallestStepIntersection travelled1 travelled2 intersections =
    let
        steps1 =
            mapWithSteps travelled1 |> filterSteps intersections

        steps2 =
            mapWithSteps travelled2 |> filterSteps intersections
    in
    List.map
        (\item ->
            let
                match1 =
                    List.filter (\( _, coord ) -> coord == item) steps1 |> List.head

                match2 =
                    List.filter (\( _, coord ) -> coord == item) steps2 |> List.head
            in
            case ( match1, match2 ) of
                ( Just ( step1, _ ), Just ( step2, _ ) ) ->
                    step1 + step2

                _ ->
                    0
        )
        intersections
        |> List.filter (\item -> item /= 0)
        |> List.minimum


solution : Solution
solution input =
    let
        maybeWires =
            Parser.run inputParser input
    in
    case maybeWires of
        Ok ( wire1, wire2 ) ->
            let
                travelled1 =
                    travelToPaths wire1 ( 0, 0 )

                travelled2 =
                    travelToPaths wire2 ( 0, 0 )

                intersections =
                    getIntersections travelled1 travelled2

                maybePart1 =
                    intersections |> distanceOfClosestIntersection

                maybePart2 =
                    distanceOfSmallestStepIntersection travelled1 travelled2 intersections

                toStringHelper maybeInt =
                    maybeInt |> Maybe.map String.fromInt |> Result.fromMaybe "an error occured"
            in
            Computed ( maybePart1 |> toStringHelper, maybePart2 |> toStringHelper )

        _ ->
            Computed ( Err "input parsing error", Err "input parsing error" )
