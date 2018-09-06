module Main exposing (Msg(..), main, update, view)

import Html exposing (Html, button, div, form, input, label, program, text)
import Html.Attributes exposing (id, type_)
import Html.Events exposing (onClick, onInput, onSubmit)


init : ( Model, Cmd Msg )
init =
    ( { username = ""
      , entered = False
      }
    , Cmd.none
    )


main : Program Never Model Msg
main =
    program { init = init, update = update, view = view, subscriptions = subscriptions }


type alias Model =
    { username : String
    , entered : Bool
    }


type Msg
    = NameChange String
    | EnterChat


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NameChange name ->
            ( { model | username = name }, Cmd.none )

        EnterChat ->
            ( { model | entered = True }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ form [ onSubmit EnterChat ]
            [ label []
                [ text "Name: "
                , input [ id "username", type_ "text", onInput NameChange ] []
                ]
            , text " "
            , button [ type_ "submit" ] [ text "Enter chat" ]
            ]
        , div []
            [ text model.username
            , text "Entered: "
            , text (toString model.entered)
            ]
        ]
