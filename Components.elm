module Components exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Util exposing (..)
import Docker.Types exposing (..)
import Components.Networks as Networks
import Messages exposing (Msg(..))


statusString : String -> String -> String
statusString state desiredState =
    if state == desiredState then
        state
    else
        state ++ " → " ++ desiredState


task : Service -> AssignedTask -> Html msg
task service { status, desiredState, containerSpec, slot } =
    let
        classes =
            [ ( status.state, True )
            , ( "desired-" ++ desiredState, True )
            , ( "running-old", status.state == "running" && service.containerSpec.image /= containerSpec.image )
            ]

        slotLabel slot =
            case slot of
                Just s ->
                    "." ++ toString s

                Nothing ->
                    ""
    in
        li [ classList classes ]
            [ text (service.name ++ slotLabel slot)
            , br [] []
            , text (statusString status.state desiredState)
            ]


serviceNode : Service -> TaskIndex -> Node -> Html msg
serviceNode service taskAllocations node =
    let
        tasks =
            Maybe.withDefault [] (Dict.get ( node.id, service.id ) taskAllocations)
    in
        td []
            [ ul [] (List.map (task service) tasks) ]


serviceRow : List Node -> TaskIndex -> Networks.Connections -> Service -> Html msg
serviceRow nodes taskAllocations networkConnections service =
    tr []
        (th [] [ text service.name ] :: (Networks.connections service networkConnections) :: (List.map (serviceNode service taskAllocations) nodes))


node : Node -> Html msg
node node =
    let
        leader =
            Maybe.withDefault False (Maybe.map .leader node.managerStatus)

        classes =
            [ ( "down", node.status.state == "down" )
            , ( "manager", node.role == "manager" )
            , ( "leader", leader )
            ]

        nodeRole =
            String.join " " [ node.role, iff leader "(leader)" "" ]
    in
        th [ classList classes ]
            [ strong [] [ text node.name ]
            , br [] []
            , text nodeRole
            , br [] []
            , text node.status.address
            ]


swarmHeader : List Node -> List Network -> String -> Html Msg
swarmHeader nodes networks pageFilter =
    tr [] ((th [] [ img [ src "docker_logo.svg" ] []
          , br [] []
          , input [ placeholder "filter", onInput PageFilter ] []]) ::
        Networks.header networks :: (nodes |> List.map node))


swarmGrid : List Service -> List Node -> List Network -> TaskIndex -> String -> Html Msg
swarmGrid services nodes networks taskAllocations pageFilter =
    let
        filteredServices =
            List.filter (\s -> (String.contains pageFilter s.name)) services
        networkConnections =
            Networks.buildConnections filteredServices networks
    in
        table []
            [ thead [] [ swarmHeader nodes networks pageFilter ]
            , tbody [] (List.map (serviceRow nodes taskAllocations networkConnections) filteredServices)
            ]
