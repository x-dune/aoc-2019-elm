module Page.Aoc exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onClick, onInput)
import Util.Helpers exposing (SolutionOutput)


type alias Model =
    { input : String
    , output : SolutionOutput
    }


type Msg
    = OnChangeInput String
    | OnClickCompute


init : Model
init =
    { input = ""
    , output = SolutionOutput "" ""
    }


view : Model -> Html Msg
view model =
    div []
        [ div [ class "flex justify-center" ]
            [ textarea
                [ onInput OnChangeInput
                , value model.input
                , class "text-white bg-black border-b-2 border-green-500 resize-none h-32 w-64"
                ]
                []
            , button
                [ onClick OnClickCompute
                , class "bg-red-600 self-end py-4 px-6 rounded-full font-bold"
                ]
                [ text "Compute" ]
            ]
        , p [] [ span [] [ text "Output 1: " ], span [] [ text model.output.part1 ] ]
        , p [] [ span [] [ text "Output 2: " ], span [] [ text model.output.part2 ] ]
        ]


createUpdate : (String -> SolutionOutput) -> (Msg -> Model -> Model)
createUpdate solution =
    \msg model ->
        case msg of
            OnChangeInput input ->
                { model | input = input }

            OnClickCompute ->
                { model | output = solution model.input }
