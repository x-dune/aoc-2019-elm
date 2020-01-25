module Solution.Day1 exposing (solution, title)

import Solution.Types exposing (Solution)


title : String
title =
    "Day 1: The Tyranny of the Rocket Equation"


part1 : List String -> Int
part1 inputs =
    inputs
        |> List.map mapMassToFuelMass
        |> List.foldl (+) 0


massToFuelMass : Int -> Int
massToFuelMass int =
    int // 3 - 2


mapMassToFuelMass : String -> Int
mapMassToFuelMass item =
    let
        maybeInt =
            String.trim item |> String.toInt
    in
    case maybeInt of
        Just int ->
            massToFuelMass int

        Nothing ->
            0


part2 : List String -> Int
part2 inputs =
    let
        ( totalFuelMass, _ ) =
            fuelMassToFuelMass
                ( part1 inputs
                , inputs
                    |> List.map mapMassToFuelMass
                )
    in
    totalFuelMass


fuelMassToFuelMass : ( Int, List Int ) -> ( Int, List Int )
fuelMassToFuelMass ( accFuelMass, fuelMasses ) =
    if List.isEmpty fuelMasses then
        ( accFuelMass, fuelMasses )

    else
        let
            nextFuelMasses =
                List.map massToFuelMass fuelMasses
                    |> List.filter (\item -> item >= 0)
        in
        fuelMassToFuelMass ( accFuelMass + List.foldl (+) 0 nextFuelMasses, nextFuelMasses )


solution : Solution
solution input =
    let
        processedInput =
            String.split "\n" input
    in
    { part1 = part1 processedInput |> String.fromInt
    , part2 = part2 processedInput |> String.fromInt
    }
