module Main exposing (Msg(..), main, update, view)

import Browser
import Debug
import Html exposing (Html, button, div, form, input, label, text)
import Html.Attributes exposing (id, type_)
import Html.Events exposing (onClick, onInput, onSubmit)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { username = ""
      , entered = False
      }
    , Cmd.none
    )


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


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
                [ text "Navn: "
                , input [ id "username", type_ "text", onInput NameChange ] []
                ]
            ]
        , div []
            [ text model.username
            , text "Entered: "
            , text (Debug.toString model.entered)
            ]
        ]
