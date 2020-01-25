module Solution.Types exposing (PageMetadata, Solution, SolutionOutput)


type alias SolutionOutput =
    { part1 : String
    , part2 : String
    }


type alias Solution =
    String -> SolutionOutput


type alias PageMetadata =
    { title : String
    , sourceCodeLink : String
    , aocPuzzleLink : String
    }
