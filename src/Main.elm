module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (class, href)
import Page.Aoc
import Page.NotFound
import Route exposing (Route)
import Solution.Aoc1
import Solution.Aoc2
import Url exposing (Url)


type Page
    = NotFoundPage
    | Aoc1Page Page.Aoc.Model
    | Aoc2Page Page.Aoc.Model


type alias Model =
    { route : Route
    , page : Page
    , navKey : Nav.Key
    }


type Msg
    = LinkClicked UrlRequest
    | UrlChanged Url
    | Aoc1PageMsg Page.Aoc.Msg
    | Aoc2PageMsg Page.Aoc.Msg


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    initCurrentPage
        ( { route = Route.parseUrl url
          , page = NotFoundPage
          , navKey = navKey
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

                Route.Aoc1 ->
                    ( Aoc1Page Page.Aoc.init, Cmd.none )

                Route.Aoc2 ->
                    ( Aoc2Page Page.Aoc.init, Cmd.none )
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

        ( Aoc1PageMsg subMsg, Aoc1Page pageModel ) ->
            let
                subUpdate =
                    Page.Aoc.createUpdate Solution.Aoc1.solution

                updatedPageModel =
                    subUpdate subMsg pageModel
            in
            ( { model | page = Aoc1Page updatedPageModel }
            , Cmd.none
            )

        ( Aoc2PageMsg subMsg, Aoc2Page pageModel ) ->
            let
                subUpdate =
                    Page.Aoc.createUpdate Solution.Aoc2.solution

                updatedPageModel =
                    subUpdate subMsg pageModel
            in
            ( { model | page = Aoc2Page updatedPageModel }
            , Cmd.none
            )

        ( _, _ ) ->
            ( model, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "Advent Of Code 2019 | Elm Solutions"
    , body =
        [ div []
            [ a [ href "/Aoc1", class "m-10" ] [ text "/Aoc1" ]
            , a [ href "/Aoc2", class "m-10" ] [ text "/Aoc2" ]
            , a [ href "/totallynotapage", class "m-10" ] [ text "/totallynotapage" ]
            ]
        , bodyView model
        ]
    }


bodyView : Model -> Html Msg
bodyView model =
    case model.page of
        NotFoundPage ->
            Page.NotFound.view

        Aoc1Page aocModel ->
            Page.Aoc.view aocModel
                |> Html.map Aoc1PageMsg

        Aoc2Page aocModel ->
            Page.Aoc.view aocModel
                |> Html.map Aoc2PageMsg


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
