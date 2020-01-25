module Page.Aoc exposing (Model, Msg, createUpdate, init, view)

import Html exposing (..)
import Html.Attributes exposing (autofocus, class, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Solution.Types exposing (Solution, SolutionOutput)


type alias Model =
    { input : String
    , output : SolutionOutput
    , title : String
    }


type Msg
    = OnChangeInput String
    | OnClickCompute


init : String -> Model
init title =
    { input = ""
    , output = SolutionOutput "" ""
    , title = title
    }


view : Model -> Html Msg
view model =
    div [ class "h-full flex-col" ]
        [ div [ class "flex-col flex-1" ]
            [ div [ class "" ]
                [ button
                    [ class "bg-gray-800 p-4 mx-2 border-green-400 border-t-4 text-green-400"
                    , onClick OnClickCompute
                    ]
                    [ text "compute" ]
                , button
                    [ class "bg-gray-800 p-4 mx-2 border-green-400 border-t-4 text-green-400" ]
                    [ text "sourceCode" ]
                , button
                    [ class "bg-gray-800 p-4 mx-2 border-green-400 border-t-4 text-green-400" ]
                    [ text "aocPuzzle" ]
                ]
            , div [ class "flex-2 border-b-4 border-gray-900" ]
                [ textarea
                    [ onInput OnChangeInput
                    , autofocus True
                    , value model.input
                    , class "text-white bg-black resize-none flex-1 p-4 font-mono"
                    , placeholder
                        "enter your input here"
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
    div [ class "flex-col font-mono" ]
        [ span [] [ text "the computed outputs are ~>" ]
        , span [ class "mr-2" ] [ text "part_1:", span [ class "text-green-500 ml-2" ] [ text model.output.part1 ] ]
        , span [ class "mr-2" ] [ text "part_2:", span [ class "text-green-500 ml-2" ] [ text model.output.part2 ] ]
        ]


createUpdate : Solution -> (Msg -> Model -> Model)
createUpdate solution =
    \msg model ->
        case msg of
            OnChangeInput input ->
                { model | input = input }

            OnClickCompute ->
                { model | output = solution model.input }
