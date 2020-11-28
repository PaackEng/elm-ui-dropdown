module Example5 exposing (..)

import Browser exposing (sandbox)
import Dropdown
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)


main : Program () Model Msg
main =
    sandbox { init = init, view = view, update = update }


type alias Model =
    { dropdownState : Dropdown.State String
    , selectedOption : Maybe String
    }


init : Model
init =
    { dropdownState = Dropdown.init "custom-dropdown"
    , selectedOption = Nothing
    }


options : List String
options =
    List.range 1 10 |> List.map (\item -> "Option " ++ String.fromInt item)



-- UPDATE


type Msg
    = OptionPicked (Maybe String)
    | ChechboxChecked Bool
    | DropdownMsg (Dropdown.Msg String)


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChechboxChecked checked ->
            model

        -- Do something fancy with the checkex option
        OptionPicked option ->
            { model | selectedOption = option }

        DropdownMsg subMsg ->
            let
                ( state, _ ) =
                    Dropdown.update dropdownConfig subMsg model.dropdownState options
            in
            { model | dropdownState = state }



-- VIEW


view : Model -> Html Msg
view model =
    Dropdown.view dropdownConfig model.dropdownState options
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


dropdownConfig : Dropdown.Config String Msg
dropdownConfig =
    let
        arrow icon =
            el [ Font.size 7, paddingEach { edges | left = 5 } ] icon

        itemToElement selected highlighted item =
            Input.checkbox []
                { onChange = ChechboxChecked
                , icon = Input.defaultCheckbox
                , checked = selected
                , label = Input.labelRight [ paddingEach { edges | left = 7 } ] <| text item
                }
    in
    Dropdown.custom
        { closeButton = arrow (text "▲")
        , containerAttributes = []
        , dropdownMsg = DropdownMsg
        , itemToElement = itemToElement
        , itemToPrompt = always btn
        , listAttributes =
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
        , onSelectMsg = OptionPicked
        , openButton = arrow (text "▼")
        , promptElement = el [ width fill ] btn
        , searchAttributes = []
        , selectAttributes =
            [ pointer
            , paddingXY 13 7
            , Background.color (rgb255 224 228 237)
            , Border.rounded 15
            , Font.letterSpacing 1
            , Font.size 16
            , Element.focused
                [ Background.color (rgb255 25 45 91), Font.color white ]
            ]
        }