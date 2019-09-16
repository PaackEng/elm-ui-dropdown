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
    { selectedOption : Maybe String
    , isOpen : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { selectedOption = Nothing
      , isOpen = False
      }
    , Cmd.none
    )


options : List String
options =
    [ "Option 1", "Option 2", "Option 3" ]



-- UPDATE


type Msg
    = ToggleDropdown
    | OptionPicked String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleDropdown ->
            ( { model | isOpen = not model.isOpen }, Cmd.none )

        OptionPicked option ->
            ( { model
                | selectedOption = Just option
                , isOpen = False
              }
            , Cmd.none
            )



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
            , isOpen = model.isOpen
            , filterText = ""
            }

        config =
            Dropdown.basic ToggleDropdown OptionPicked
                |> Dropdown.withItemToElement Element.text
    in
    Dropdown.view config state options
        |> el []
        |> layout []
