module Solution.Types exposing (Solution, SolutionOutput)


type alias SolutionOutput =
    { part1 : String
    , part2 : String
    }


type alias Solution =
    String -> SolutionOutput
