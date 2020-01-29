module Route exposing (Route(..), parseUrl)

import Url exposing (Url)
import Url.Parser exposing (Parser, map, oneOf, parse, s, top)


type Route
    = NotFound
    | Day1
    | Day2
    | Day3


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
        [ map Day1 top
        , map Day1 (s "day1")
        , map Day2 (s "day2")
        , map Day3 (s "day3")
        ]
