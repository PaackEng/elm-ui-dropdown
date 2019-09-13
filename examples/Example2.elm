module Example2 exposing (..)

import Browser
import Dropdown
import Element exposing (..)
import Element.Border as Border
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
            }

        containerAttrs =
            [ centerX, centerY ]

        inputAttrs =
            [ Border.width 1, Border.rounded 5, paddingXY 16 8 ]

        textAttrs =
            [ paddingXY 8 0 ]

        listAttrs =
            [ Border.width 1, width fill, spacing 5 ]

        listItemAttrs =
            [ Font.size 16, centerX, padding 8 ]

        dropdown =
            Dropdown.config ToggleDropdown OptionPicked
                |> Dropdown.withContainerAttributes containerAttrs
                |> Dropdown.withInputAttributes inputAttrs
                |> Dropdown.withTextAttributes textAttrs
                |> Dropdown.withListAttributes listAttrs
                |> Dropdown.withListItemAttributes listItemAttrs
                |> Dropdown.toEl state options
    in
    el [ width fill, height fill, padding 20 ] dropdown
        |> layout []
