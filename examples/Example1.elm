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
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { dropdownState = Dropdown.init
      }
    , Cmd.none
    )


options : List String
options =
    [ "Option 1", "Option 2", "Option 3" ]



-- UPDATE


type Msg
    = OptionPicked String
    | DropdownMsg (Dropdown.Msg String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OptionPicked option ->
            ( { model
                | selectedOption = Just option
                , isOpen = False
              }
            , Cmd.none
            )

        DropdownMsg subMsg ->
            Dropdown.update dropdownConfig subMsg model.dropdownState



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        state =
            { selectedItem = model.selectedOption
            , filterText = ""
            }
    in
    Dropdown.view dropdownConfig state options
        |> el []
        |> layout []


dropdownConfig =
    Dropdown.basic ToggleDropdown OptionPicked
        |> Dropdown.withItemToElement Element.text
