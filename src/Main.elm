module Main exposing (Msg(..), main, update, view)

import Dict exposing (Dict)
import Html exposing (Html, button, div, form, input, label, program, text)
import Html.Attributes exposing (id, type_)
import Html.Events exposing (onClick, onInput, onSubmit)
import WebSocket exposing (listen, send)


init : ( Model, Cmd Msg )
init =
    ( { username = ""
      , entered = False
      , channels = Dict.empty
      }
    , Cmd.none
    )


main : Program Never Model Msg
main =
    program { init = init, update = update, view = view, subscriptions = subscriptions }


type alias Model =
    { username : String
    , entered : Bool
    , channels : Dict String (List String)
    }


type alias Channel =
    String


type Msg
    = NameChange String
    | EnterChat
    | JoinChannel Channel
    | ReceivedMsg Channel String
    | SendMsg Channel String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NameChange name ->
            ( { model | username = name }, Cmd.none )

        EnterChat ->
            ( { model | entered = True }, send websocketUrl model.username )

        JoinChannel channel ->
            ( model, send websocketUrl ("/join #" ++ channel) )

        ReceivedMsg channel content ->
            ( model, Cmd.none )

        SendMsg channel content ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


handleMsg : String -> String -> Msg
handleMsg username rawMsg =
    if rawMsg == "welcome " ++ username then
        JoinChannel "finn"

    else
        ReceivedMsg "" ""


websocketUrl : String
websocketUrl =
    "ws://localhost:9160"


subscriptions : Model -> Sub Msg
subscriptions model =
    listen websocketUrl (handleMsg model.username)


view : Model -> Html Msg
view model =
    if model.entered then
        div []
            [ text ("Your name is " ++ model.username)
            , form [] []
            ]

    else
        form [ onSubmit EnterChat ]
            [ label []
                [ text "Name: "
                , input [ id "username", type_ "text", onInput NameChange ] []
                ]
            , text " "
            , button [ type_ "submit" ] [ text "Enter chat" ]
            ]
