module Solution.Types exposing (PageMetadata, Solution, SolutionOutput(..))


type SolutionOutput
    = Computed ( Result String String, Result String String )
    | NotComputed


type alias Solution =
    String -> SolutionOutput


type alias PageMetadata =
    { title : String
    , sourceCodeLink : String
    , aocPuzzleLink : String
    }
