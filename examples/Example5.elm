module Example5 exposing (..)

import Browser exposing (element)
import Dropdown
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)


main : Program () Model Msg
main =
    element { init = init, view = view, update = update, subscriptions = subscriptions }


subscriptions : Model -> Sub Msg
subscriptions model =
    Dropdown.onOutsideClick model.dropdownState DropdownMsg


type alias Model =
    { dropdownState : Dropdown.State
    , selectedOptions : List String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { dropdownState = Dropdown.init "my-dropdown", selectedOptions = options |> List.take 2 }, Cmd.none )


options : List String
options =
    List.range 1 10 |> List.map (\item -> "Option " ++ String.fromInt item)



-- UPDATE


type Msg
    = OptionsPicked (List String)
    | ChechboxChecked Bool
    | DropdownMsg (Dropdown.Msg String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChechboxChecked checked ->
            ( model, Cmd.none )

        -- Do something fancy with the checkex option
        OptionsPicked selectedOptions ->
            ( { model | selectedOptions = selectedOptions }, Cmd.none )

        DropdownMsg subMsg ->
            let
                ( state, cmd ) =
                    Dropdown.update dropdownConfig subMsg model model.dropdownState
            in
            ( { model | dropdownState = state }, cmd )



-- VIEW


view : Model -> Html Msg
view model =
    Dropdown.view dropdownConfig model model.dropdownState
        |> el []
        |> layout []


edges : { top : number, right : number, bottom : number, left : number }
edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


white : Color
white =
    rgb255 255 255 255


lightGrey : Color
lightGrey =
    rgb255 155 155 155


btn : Element a
btn =
    Input.button
        [ alignTop
        , alignRight
        , Element.focused [ Background.color (rgb255 25 45 91), Font.color white ]
        ]
        { label = el [] (text "COMPARE")
        , onPress = Nothing
        }


dropdownConfig : Dropdown.Config String Msg Model
dropdownConfig =
    let
        arrow icon =
            el [ Font.size 7, paddingEach { edges | left = 5 } ] icon

        listAttrs =
            [ Background.color white
            , Border.rounded 5
            , padding 20
            , spacing 20
            , alignLeft
            , height (px 220)
            , scrollbarX
            , Border.shadow
                { offset = ( 0, 4 )
                , size = 0.1
                , blur = 6
                , color = lightGrey
                }
            ]

        selectAttrs =
            [ pointer
            , paddingXY 13 7
            , Background.color (rgb255 224 228 237)
            , Border.rounded 15
            , Font.letterSpacing 1
            , Font.size 16
            , Element.focused
                [ Background.color (rgb255 25 45 91), Font.color white ]
            ]

        itemsToPrompt =
            always btn

        itemToElement selected highlighted item =
            Input.checkbox []
                { onChange = ChechboxChecked
                , icon = Input.defaultCheckbox
                , checked = selected
                , label = Input.labelRight [ paddingEach { edges | left = 7 } ] <| text item
                }
    in
    Dropdown.multi
        { allItems = always options
        , selectedItems = .selectedOptions
        , dropdownMsg = DropdownMsg
        , onSelectMsg = OptionsPicked
        , itemsToPrompt = itemsToPrompt
        , itemToElement = itemToElement
        }
        |> Dropdown.withPromptElement btn
        |> Dropdown.withListAttributes listAttrs
        |> Dropdown.withSelectAttributes selectAttrs
        |> Dropdown.withOpenCloseButtons { openButton = arrow (text "▼"), closeButton = arrow (text "▲") }
