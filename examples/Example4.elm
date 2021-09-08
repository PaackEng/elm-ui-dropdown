module Example4 exposing (..)

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
    List.range 1 100
        |> List.map String.fromInt
        |> List.map ((++) "Option ")



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
    column [ padding 20, spacing 20 ]
        [ el [] <| text <| "Selected Option: " ++ (model.selectedOption |> Maybe.withDefault "Nothing")
        , Dropdown.view dropdownConfig model model.dropdownState
        , el [] <| text "other content"
        ]
        |> layout []


dropdownConfig : Dropdown.Config Item Msg Model
dropdownConfig =
    let
        containerAttrs =
            [ width (px 200) ]

        selectAttrs =
            [ Border.width 1
            , Border.rounded 5
            , paddingXY 16 8
            , spacing 10
            , width fill
            ]

        searchAttrs =
            [ Border.width 0, padding 0, width fill ]

        listAttrs =
            [ Border.width 1
            , Border.roundEach { topLeft = 0, topRight = 0, bottomLeft = 5, bottomRight = 5 }
            , width fill
            , clip
            , scrollbarY
            , height (fill |> maximum 200)
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

        emptyListElement =
            Element.el
                [ Element.width fill
                , Font.size 12
                , Element.padding 8
                , Background.color <| rgb255 255 220 220
                ]
            <|
                Element.text "Nothing matches..."
    in
    Dropdown.filterable
        { itemsFromModel = always options
        , selectionFromModel = .selectedOption
        , dropdownMsg = DropdownMsg
        , onSelectMsg = OptionPicked
        , itemToPrompt = itemToPrompt
        , itemToElement = itemToElement
        , itemToText = identity
        }
        |> Dropdown.withEmptyListElement emptyListElement
        |> Dropdown.withContainerAttributes containerAttrs
        |> Dropdown.withSelectAttributes selectAttrs
        |> Dropdown.withListAttributes listAttrs
        |> Dropdown.withSearchAttributes searchAttrs
