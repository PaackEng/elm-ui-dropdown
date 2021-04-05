module Example1 exposing (..)

import Browser
import Dropdown
import Element exposing (..)
import Html exposing (Html)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Item =
    String


type alias Model =
    { dropdownState : Dropdown.State Item
    , selectedOption : Maybe Item
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { dropdownState = Dropdown.init "dropdown"
      , selectedOption = Nothing
      }
    , Cmd.none
    )


options : List Item
options =
    [ "Option 1", "Option 2", "Option 3" ]



-- UPDATE


type Msg
    = OptionPicked (Maybe Item)
    | DropdownMsg (Dropdown.Msg Item)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OptionPicked option ->
            ( { model | selectedOption = option }, Cmd.none )

        DropdownMsg subMsg ->
            let
                ( state, cmd ) =
                    Dropdown.update dropdownConfig subMsg model model.dropdownState
            in
            ( { model | dropdownState = state }, cmd )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    Dropdown.view dropdownConfig model model.dropdownState
        |> el []
        |> layout []


dropdownConfig : Dropdown.Config Item Msg Model
dropdownConfig =
    let
        itemToPrompt item =
            text item

        itemToElement selected highlighted item =
            text item
    in
    Dropdown.basic
        { itemsFromModel = always options
        , selectionFromModel = .selectedOption
        , dropdownMsg = DropdownMsg
        , onSelectMsg = OptionPicked
        , itemToPrompt = itemToPrompt
        , itemToElement = itemToElement
        }
