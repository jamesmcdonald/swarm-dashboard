module Main exposing (..)

import Navigation
import Html exposing (..)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onInput)
import Dict exposing (Dict)
import Util exposing (..)
import WebSocket
import Docker.Types exposing (..)
import Docker exposing (fromJson)
import Components as UI


localWebsocket : Navigation.Location -> String
localWebsocket location =
    if location.protocol == "https:" then
        "wss://" ++ location.host ++ "/stream"
    else
        "ws://" ++ location.host ++ "/stream"


type alias Model =
    { webSocketUrl : String
    , swarm : Docker
    , tasks : TaskIndex
    , errors : List String
    , pageFilter : String
    }


type Msg
    = UrlChange Navigation.Location
    | Receive String
    | PageFilter String


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( { webSocketUrl = localWebsocket location
      , swarm = Docker.empty
      , tasks = Dict.empty
      , errors = []
      , pageFilter = ""
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Receive serverJson ->
            case fromJson serverJson of
                Ok serverData ->
                    ( { model | swarm = serverData, tasks = groupBy taskIndexKey serverData.assignedTasks }, Cmd.none )

                Err error ->
                    ( { model | errors = error :: model.errors }, Cmd.none )

        UrlChange location ->
            ( model, Cmd.none )
        PageFilter pageFilter ->
            ( { model | pageFilter = pageFilter }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen model.webSocketUrl Receive

filterServices : List Service -> String -> List Service
filterServices service pageFilter =
    List.filter (\s -> (String.contains pageFilter s.name)) service


view : Model -> Html Msg
view { swarm, tasks, errors, pageFilter } =
    let
        { services, nodes, networks } =
            swarm
    in
        div []
            [ input [ placeholder "filter", onInput PageFilter ] []
            , UI.swarmGrid (filterServices services pageFilter) nodes networks tasks
            , ul [] (List.map (\e -> li [] [ text e ]) errors)
            ]


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
