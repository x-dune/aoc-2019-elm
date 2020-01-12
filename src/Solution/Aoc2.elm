module Solution.Aoc2 exposing (part2, solution)

import Array exposing (Array)
import Util.Helpers exposing (SolutionOutput)


type alias IntcodeIO =
    { output : Int
    , input : ( Int, Int )
    }


genericError : Result String a
genericError =
    Err "Error"


part1 : Array Int -> Result String Int
part1 inputArray =
    let
        result =
            processIntcodeOnce ( 12, 2 ) inputArray
    in
    case result of
        Ok actualResult ->
            Ok actualResult.output

        _ ->
            genericError


processIntcodeOnce : ( Int, Int ) -> Array Int -> Result String IntcodeIO
processIntcodeOnce ( noun, verb ) inputArray =
    let
        replacedInputArray =
            inputArray
                |> Array.set 1 noun
                |> Array.set 2 verb

        ( _, outputArray ) =
            intcodeComputer ( 0, replacedInputArray )
    in
    case Array.get 0 outputArray of
        Just output ->
            Ok <| IntcodeIO output ( noun, verb )

        _ ->
            genericError


intcodeComputer : ( Int, Array Int ) -> ( Int, Array Int )
intcodeComputer ( index, inputArray ) =
    let
        maybeOpcode =
            Array.get index inputArray

        ( maybePos1, maybePos2, maybePos3 ) =
            ( Array.get (index + 1) inputArray
            , Array.get (index + 2) inputArray
            , Array.get (index + 3) inputArray
            )

        ( maybeVal1, maybeVal2 ) =
            ( posToVal ( maybePos1, inputArray )
            , posToVal ( maybePos2, inputArray )
            )
    in
    case [ maybeOpcode, maybeVal1, maybeVal2, maybePos3 ] of
        [ Just 1, Just val1, Just val2, Just pos3 ] ->
            let
                outputArray =
                    inputArray |> Array.set pos3 (val1 + val2)
            in
            intcodeComputer ( index + 4, outputArray )

        [ Just 2, Just val1, Just val2, Just pos3 ] ->
            intcodeComputer ( index + 4, inputArray |> Array.set pos3 (val1 * val2) )

        [ Just 99 ] ->
            ( 0, inputArray )

        _ ->
            ( 0, inputArray )


posToVal : ( Maybe Int, Array Int ) -> Maybe Int
posToVal ( maybePos, inputArray ) =
    Maybe.andThen (\pos -> Array.get pos inputArray) maybePos


part2 : Array Int -> Result String Int
part2 inputArray =
    let
        listOfPos =
            List.range 1 99

        maybeResult =
            listOfPos
                |> List.concatMap (\item -> List.map (\innerItem -> ( item, innerItem )) listOfPos)
                |> List.map (\item -> processIntcodeOnce item inputArray)
                |> List.filter
                    (\item ->
                        case item of
                            Ok actualItem ->
                                actualItem.output == 19690720

                            _ ->
                                False
                    )
                |> List.head
    in
    case maybeResult of
        Just (Ok output) ->
            Ok <| Tuple.first output.input * 100 + Tuple.second output.input

        _ ->
            genericError


solution : String -> SolutionOutput
solution input =
    let
        processedInput =
            String.split "," input
                |> List.filterMap String.toInt
                |> Array.fromList

        parseResultToString result =
            case result of
                Ok actualResult ->
                    String.fromInt actualResult

                Err errorMessage ->
                    errorMessage
    in
    { part1 = part1 processedInput |> parseResultToString
    , part2 = part2 processedInput |> parseResultToString
    }
