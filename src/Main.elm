module Main exposing (Msg(..), main, update, view)

import Dict exposing (Dict, insert)
import Html exposing (Html, button, div, form, input, label, program, text)
import Html.Attributes exposing (id, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Regex
import WebSocket exposing (listen, send)


init : ( Model, Cmd Msg )
init =
    ( { username = ""
      , entered = False
      , channels = Dict.empty
      , maybeChannel = Nothing
      }
    , Cmd.none
    )


main : Program Never Model Msg
main =
    program { init = init, update = update, view = view, subscriptions = subscriptions }


type alias Model =
    { username : String
    , entered : Bool
    , channels : Dict String Channel
    , maybeChannel : Maybe ChannelName
    }


addChannel : Channel -> Model -> Model
addChannel ch model =
    { model | channels = insert ch.name ch model.channels }


addMessageToModel : ChannelName -> String -> Model -> Model
addMessageToModel ch msg model =
    { model | channels = Dict.update ch (addMessageToChannel msg) model.channels }


addMessageToChannel : String -> Maybe Channel -> Maybe Channel
addMessageToChannel msg maybeChannel =
    case maybeChannel of
        Just channel ->
            Just { channel | messageLog = msg :: channel.messageLog }

        Nothing ->
            Nothing


newChannel : String -> Channel
newChannel name =
    { name = name
    , input = ""
    , messageLog = []
    }


setCurrentChannel : ChannelName -> Model -> Model
setCurrentChannel ch model =
    { model | maybeChannel = Just ch }


getCurrentChannel : Model -> Maybe Channel
getCurrentChannel model =
    case model.maybeChannel of
        Just ch ->
            Dict.get ch model.channels

        _ ->
            Nothing


updateChannelInput : ChannelName -> String -> Model -> Model
updateChannelInput channelName value model =
    case Dict.get channelName model.channels of
        Just channel ->
            let
                updatedChannel =
                    { channel | input = value }
            in
            addChannel updatedChannel model

        _ ->
            model


clearChannelInput : ChannelName -> Model -> Model
clearChannelInput channelName =
    updateChannelInput channelName ""


type alias Channel =
    { name : ChannelName
    , input : String
    , messageLog : List String
    }


type alias ChannelName =
    String


type Msg
    = NameChange String
    | EnterChat
    | JoinChannel ChannelName
    | ChatInputChange ChannelName String
    | ReceivedMsg ChannelName String
    | SendMsg ChannelName String
    | NothingHappened


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NameChange name ->
            ( { model | username = name }, Cmd.none )

        EnterChat ->
            ( { model | entered = True }, send websocketUrl model.username )

        ChatInputChange channelName value ->
            ( updateChannelInput channelName value model, Cmd.none )

        JoinChannel channelName ->
            ( addChannel (newChannel channelName) model
                |> setCurrentChannel channelName
            , send websocketUrl ("/join #" ++ channelName)
            )

        ReceivedMsg channelName content ->
            ( addMessageToModel channelName content model, Cmd.none )

        SendMsg channelName value ->
            case Dict.get channelName model.channels of
                Just channel ->
                    ( clearChannelInput channel.name model, send websocketUrl ("#" ++ channelName ++ " " ++ model.username ++ ": " ++ value) )

                _ ->
                    ( model, Cmd.none )

        NothingHappened ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


handleMsg : String -> String -> Msg
handleMsg username rawMsg =
    if rawMsg == "welcome " ++ username then
        JoinChannel "finn"

    else
        let
            matches =
                Regex.find Regex.All (Regex.regex "#([^ ]*) (.*)") rawMsg
        in
        case matches of
            [] ->
                NothingHappened

            match :: _ ->
                case match.submatches of
                    [ Just channelName, Just content ] ->
                        ReceivedMsg channelName content

                    _ ->
                        NothingHappened


websocketUrl : String
websocketUrl =
    "ws://localhost:9160"


subscriptions : Model -> Sub Msg
subscriptions model =
    listen websocketUrl (handleMsg model.username)


handleSubmit : ChannelName -> String -> Msg
handleSubmit ch value =
    if String.isEmpty (String.trim value) then
        NothingHappened

    else
        let
            matches =
                Regex.find Regex.All (Regex.regex "/join #([a-zA-Z0-9-_]+)") value
        in
        case matches of
            match :: _ ->
                case match.submatches of
                    [ Just channelName ] ->
                        JoinChannel channelName

                    _ ->
                        NothingHappened

            _ ->
                SendMsg ch value


view : Model -> Html Msg
view model =
    if model.entered then
        case getCurrentChannel model of
            Just channel ->
                div []
                    [ text ("Your name is " ++ model.username)
                    , form [ onSubmit (handleSubmit channel.name channel.input) ]
                        [ input [ value channel.input, onInput (ChatInputChange channel.name), type_ "text" ] []
                        , text " "
                        , button []
                            [ text "Send"
                            ]
                        ]
                    , div []
                        (List.map
                            (\x -> div [] [ text x ])
                            channel.messageLog
                        )
                    ]

            _ ->
                text ""

    else
        form [ onSubmit EnterChat ]
            [ label []
                [ text "Name: "
                , input [ id "username", type_ "text", onInput NameChange ] []
                ]
            , text " "
            , button [ type_ "submit" ] [ text "Enter chat" ]
            ]
