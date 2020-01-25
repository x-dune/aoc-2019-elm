module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (class, href, src)
import Page.Aoc
import Page.NotFound
import Route exposing (Route)
import Solution.Day1
import Solution.Day2
import Solution.Types exposing (Solution)
import Url exposing (Url)


type Page
    = NotFoundPage
    | AocPage Page.Aoc.Model Solution


type alias Model =
    { route : Route
    , page : Page
    , navKey : Nav.Key
    , logoPath : String
    }


type Msg
    = LinkClicked UrlRequest
    | UrlChanged Url
    | AocPageMsg Page.Aoc.Msg


type alias Flags =
    String


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    initCurrentPage
        ( { route = Route.parseUrl url
          , page = NotFoundPage
          , navKey = navKey
          , logoPath = flags
          }
        , Cmd.none
        )


initCurrentPage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
initCurrentPage ( model, existingCmds ) =
    let
        ( currentPage, mappedPageCmds ) =
            case model.route of
                Route.NotFound ->
                    ( NotFoundPage, Cmd.none )

                Route.Day1 ->
                    ( AocPage (Page.Aoc.init Solution.Day1.title) Solution.Day1.solution
                    , Cmd.none
                    )

                Route.Day2 ->
                    ( AocPage (Page.Aoc.init Solution.Day2.title) Solution.Day2.solution
                    , Cmd.none
                    )
    in
    ( { model | page = currentPage }
    , Cmd.batch [ existingCmds, mappedPageCmds ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navKey (Url.toString url) )

                Browser.External url ->
                    ( model, Nav.load url )

        ( UrlChanged url, _ ) ->
            initCurrentPage <|
                ( { model | route = Route.parseUrl url }, Cmd.none )

        ( AocPageMsg subMsg, AocPage pageModel pageSolution ) ->
            let
                subUpdate =
                    Page.Aoc.createUpdate pageSolution

                updatedPageModel =
                    subUpdate subMsg pageModel
            in
            ( { model | page = AocPage updatedPageModel pageSolution }
            , Cmd.none
            )

        ( _, _ ) ->
            ( model, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "Advent Of Code 2019 | Elm Solutions"
    , body =
        [ div []
            [ viewSidebar model
            , div
                [ class "flex-col flex-1" ]
                [ viewTitle model
                , viewBody model
                ]
            ]
        ]
    }


viewSidebar : Model -> Html Msg
viewSidebar model =
    div [ class "flex-col bg-indigo-600 min-h-screen" ]
        [ a [ href "/", class "m-4" ]
            [ img [ src model.logoPath, class "h-auto w-32" ] []
            ]
        , a [ href "/day1", class "mx-4 my-1" ] [ text "/day1" ]
        , a [ href "/day2", class "mx-4 my-1" ] [ text "/day2" ]
        , a [ href "/totallynotapage", class "mx-4 my-1" ] [ text "/totallynotapage" ]
        ]


viewTitle : Model -> Html Msg
viewTitle model =
    let
        pageTitle =
            case model.page of
                NotFoundPage ->
                    text ""

                AocPage aocModel _ ->
                    p [ class "mb-2 text-lg" ] [ text aocModel.title ]
    in
    div [ class "m-2 flex-col" ]
        [ p [ class "text-2xl mt-4 mb-2 font-bold" ] [ text "Advent of Code 2019 in Elm" ]
        , pageTitle
        ]


viewBody : Model -> Html Msg
viewBody model =
    case model.page of
        NotFoundPage ->
            Page.NotFound.view

        AocPage aocModel _ ->
            Page.Aoc.view aocModel
                |> Html.map AocPageMsg


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
