module Route exposing (Route(..), parseUrl)

import Url exposing (Url)
import Url.Parser exposing (Parser, map, oneOf, parse, s, top)


type Route
    = NotFound
    | Aoc1
    | Aoc2


parseUrl : Url -> Route
parseUrl url =
    case parse matchRoute url of
        Just route ->
            route

        Nothing ->
            NotFound


matchRoute : Parser (Route -> a) a
matchRoute =
    oneOf
        [ map Aoc1 top
        , map Aoc1 (s "Aoc1")
        , map Aoc2 (s "Aoc2")
        ]
