module Util.Helper exposing (metadataHelper)

import Solution.Types exposing (PageMetadata)


metadataHelper : String -> Int -> PageMetadata
metadataHelper title dayNumber =
    let
        dayNumberString =
            String.fromInt dayNumber
    in
    PageMetadata
        title
        ("https://github.com/adamdune/aoc-2019-elm/blob/master/src/Solution/Day"
            ++ dayNumberString
            ++ ".elm"
        )
        ("https://adventofcode.com/2019/day/"
            ++ dayNumberString
        )
