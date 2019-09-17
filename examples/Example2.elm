module Example2 exposing (..)

import Browser
import Dropdown
import Element exposing (..)
import Element.Background as Background
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
    { dropdownState : Dropdown.State String
    , selectedOption : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { dropdownState = Dropdown.init
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
                    Dropdown.update dropdownConfig subMsg model.dropdownState
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
        |> el [ width fill, height fill, padding 20 ]
        |> layout []


dropdownConfig : Dropdown.Config String Msg
dropdownConfig =
    let
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

        itemToElement i =
            row
                [ mouseOver [ Background.color (rgb255 128 128 128) ]
                , padding 8
                , spacing 10
                , width fill
                ]
                [ el [] (text "-")
                , el [ Font.size 16 ] (text i)
                ]
    in
    Dropdown.filterable DropdownMsg OptionPicked
        |> Dropdown.withItemToPrompt identity
        |> Dropdown.withItemToElement itemToElement
        |> Dropdown.withContainerAttributes containerAttrs
        |> Dropdown.withHeadAttributes inputAttrs
        |> Dropdown.withSearchAttributes searchAttrs
        |> Dropdown.withTextAttributes textAttrs
        |> Dropdown.withListAttributes listAttrs
