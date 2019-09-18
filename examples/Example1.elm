module Example1 exposing (..)

import Browser
import Dropdown
import Element exposing (..)
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
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { dropdownState = Dropdown.init "dropdown"
      , selectedOption = Nothing
      }
    , Cmd.none
    )


options : List String
options =
    [ "Option 1", "Option 2", "Option 3" ]



-- UPDATE


type Msg
    = OptionPicked (Maybe String)
    | DropdownMsg (Dropdown.Msg String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OptionPicked option ->
            ( { model | selectedOption = option }, Cmd.none )

        DropdownMsg subMsg ->
            let
                ( state, cmd ) =
                    Dropdown.update dropdownConfig subMsg model.dropdownState options
            in
            ( { model | dropdownState = state }, cmd )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    Dropdown.view dropdownConfig model.dropdownState options
        |> el []
        |> layout []


dropdownConfig : Dropdown.Config String Msg
dropdownConfig =
    Dropdown.basic DropdownMsg OptionPicked (\_ _ item -> Element.text item)
