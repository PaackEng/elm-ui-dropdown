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
    , filterText : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { selectedOption = Nothing
      , isOpen = False
      , filterText = ""
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
    | FilterChanged String


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

        FilterChanged val ->
            ( { model | filterText = val }, Cmd.none )



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
            , filterText = model.filterText
            }

        containerAttrs =
            [ width (px 300), centerX, centerY ]

        inputAttrs =
            [ Border.width 1, Border.rounded 5, paddingXY 16 8, width fill, spacing 10 ]

        searchAttrs =
            [ paddingXY 0 3 ]

        textAttrs =
            [ paddingXY 8 0, width fill ]

        listAttrs =
            [ Border.width 1
            , Border.roundEach { topLeft = 0, topRight = 0, bottomLeft = 5, bottomRight = 5 }
            , width fill
            , spacing 5
            ]

        listItemAttrs =
            [ Font.size 16, centerX, padding 8 ]

        data =
            options |> List.filter (String.contains model.filterText)

        dropdown =
            Dropdown.filterable ToggleDropdown FilterChanged OptionPicked
                |> Dropdown.withContainerAttributes containerAttrs
                |> Dropdown.withHeadAttributes inputAttrs
                |> Dropdown.withSearchAttributes searchAttrs
                |> Dropdown.withTextAttributes textAttrs
                |> Dropdown.withListAttributes listAttrs
                |> Dropdown.withListItemAttributes listItemAttrs
                |> Dropdown.toEl state data
    in
    el [ width fill, height fill, padding 20 ] dropdown
        |> layout []
