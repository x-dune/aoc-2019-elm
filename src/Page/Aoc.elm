module Page.Aoc exposing (Model, Msg, createUpdate, init, view)

import Html exposing (..)
import Html.Attributes exposing (autocomplete, autofocus, class, href, placeholder, spellcheck, target, value)
import Html.Events exposing (onClick, onInput)
import Solution.Types exposing (PageMetadata, Solution, SolutionOutput(..))


type alias Model =
    { input : String
    , output : SolutionOutput
    , metadata : PageMetadata
    }


type Msg
    = OnChangeInput String
    | OnClickCompute


init : PageMetadata -> Model
init metadata =
    { input = ""
    , output = NotComputed
    , metadata = metadata
    }


view : Model -> Html Msg
view model =
    div [ class "h-full flex-col" ]
        [ div [ class "flex-col flex-1" ]
            [ div [ class "" ]
                [ button
                    [ class "bg-gray-800 p-4 mx-2 border-green-600 border-t-4 text-green-400 _compute-btn relative"
                    , onClick OnClickCompute
                    ]
                    [ text "compute" ]
                , a [ href model.metadata.sourceCodeLink, target "_blank" ]
                    [ button
                        [ class "bg-gray-800 p-4 mx-2 border-green-600 border-t-4 text-green-400" ]
                        [ text "sourceCode" ]
                    ]
                , a [ href model.metadata.aocPuzzleLink, target "_blank" ]
                    [ button
                        [ class "bg-gray-800 p-4 mx-2 border-green-600 border-t-4 text-green-400"
                        ]
                        [ text "aocPuzzle" ]
                    ]
                ]
            , div [ class "flex-2 border-b-4 border-gray-900" ]
                [ textarea
                    [ onInput OnChangeInput
                    , autofocus True
                    , value model.input
                    , class "text-white bg-black resize-none flex-1 p-4 font-mono"
                    , placeholder "enter your input here"
                    , autocomplete False
                    , spellcheck False
                    ]
                    []
                ]
            , div
                [ class "flex-1 flex-col bg-black p-4" ]
                [ viewOutputText model ]
            ]
        ]


viewOutputText : Model -> Html Msg
viewOutputText model =
    let
        buildOutputText output =
            case output of
                Ok actualOutput ->
                    span [ class "text-green-500 ml-2" ] [ text actualOutput ]

                Err errMsg ->
                    span [ class "text-red-500 ml-2" ] [ text errMsg ]
    in
    case model.output of
        Computed ( part1, part2 ) ->
            div [ class "flex-col font-mono" ]
                [ span [] [ text "the computed outputs are ~>" ]
                , span [ class "mr-2" ] [ text "part_1:", buildOutputText part1 ]
                , span [ class "mr-2" ] [ text "part_2:", buildOutputText part2 ]
                ]

        NotComputed ->
            div [ class "flex-col font-mono" ] [ span [] [ text "your output goes here" ] ]


createUpdate : Solution -> (Msg -> Model -> Model)
createUpdate solution =
    \msg model ->
        case msg of
            OnChangeInput input ->
                { model | input = input }

            OnClickCompute ->
                { model | output = solution model.input }
