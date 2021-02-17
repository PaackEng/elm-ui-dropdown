module Example2 exposing (..)

import Browser
import Dropdown
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)


main : Program () Model Msg
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
    [ "Option 1", "Option 2", "Option 3", "Option 4", "Option 5", "Option 6" ]



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
    column [ padding 20, spacing 20 ]
        [ el [] <| text <| "Selected Option: " ++ (model.selectedOption |> Maybe.withDefault "Nothing")
        , Dropdown.view dropdownConfig model.dropdownState options
        ]
        |> layout []


dropdownConfig : Dropdown.Config String Msg Model
dropdownConfig =
    let
        containerAttrs =
            [ width (px 300) ]

        selectAttrs =
            [ Border.width 1, Border.rounded 5, paddingXY 16 8, spacing 10, width fill ]

        searchAttrs =
            [ Border.width 0, padding 0 ]

        listAttrs =
            [ Border.width 1
            , Border.roundEach { topLeft = 0, topRight = 0, bottomLeft = 5, bottomRight = 5 }
            , width fill
            , spacing 5
            ]

        itemToPrompt item =
            text item

        itemToElement selected highlighted i =
            let
                bgColor =
                    if highlighted then
                        rgb255 128 128 128

                    else if selected then
                        rgb255 100 100 100

                    else
                        rgb255 255 255 255
            in
            row
                [ Background.color bgColor
                , padding 8
                , spacing 10
                , width fill
                ]
                [ el [] (text "-")
                , el [ Font.size 16 ] (text i)
                ]
    in
    Dropdown.filterable (.selectedOption) DropdownMsg OptionPicked itemToPrompt itemToElement identity
        |> Dropdown.withContainerAttributes containerAttrs
        |> Dropdown.withPromptElement (el [] (text "Select option"))
        |> Dropdown.withFilterPlaceholder "Type for option"
        |> Dropdown.withSelectAttributes selectAttrs
        |> Dropdown.withListAttributes listAttrs
        |> Dropdown.withSearchAttributes searchAttrs
