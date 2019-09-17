module Example4 exposing (..)

import Browser
import Dropdown
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Html exposing (Html)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { dropdownState : Dropdown.State String
    , selectedOption : Maybe String
    , options : List String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { dropdownState = Dropdown.init
      , selectedOption = Nothing
      , options =
            [ "Option 1", "Option 2", "Option 3" ]
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = OptionPicked (Maybe String)
    | DropdownMsg (Dropdown.Msg String)
    | RemoveOptions


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OptionPicked option ->
            ( { model | selectedOption = option }, Cmd.none )

        DropdownMsg subMsg ->
            let
                ( state, cmd ) =
                    Dropdown.update dropdownConfig subMsg model.dropdownState
            in
            ( { model | dropdownState = state }, cmd )

        RemoveOptions ->
            ( { model | options = [] }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    row [ spacing 15, padding 40, Background.color gainsboro ]
        [ el [] (Dropdown.view dropdownConfig model.dropdownState model.options)
        , el [ Events.onClick RemoveOptions ] (text "Remove options")
        ]
        |> layout []


black =
    rgb255 0 0 0


gray =
    rgb255 105 105 105


gainsboro =
    rgb255 220 220 220


dropdownConfig : Dropdown.Config String Msg
dropdownConfig =
    let
        disabledAttrs =
            [ Border.color gray, Border.width 1, Font.color gray, padding 12 ]

        inputAttrs =
            [ Border.color black, Border.width 2, Font.color black, padding 12 ]
    in
    Dropdown.basic DropdownMsg OptionPicked
        |> Dropdown.withItemToPrompt identity
        |> Dropdown.withItemToElement Element.text
        |> Dropdown.withHeadAttributes inputAttrs
        |> Dropdown.withDisabledAttributes disabledAttrs
