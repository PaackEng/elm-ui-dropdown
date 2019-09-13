module Dropdown exposing (Dropdown, State, config, toEl, withContainerAttributes, withDisabledAttributes, withInputAttributes, withListAttributes, withListItemAttributes, withPromptText, withTextAttributes)

import Element exposing (..)
import Element.Events as Events


type alias State =
    { selectedItem : Maybe String
    , isOpen : Bool
    }


type alias Options msg =
    { promptText : String
    , clickedMsg : msg
    , itemPickedMsg : String -> msg
    , containerAttributes : List (Attribute msg)
    , disabledAttributes : List (Attribute msg)
    , inputAttributes : List (Attribute msg)
    , textAttributes : List (Attribute msg)
    , listAttributes : List (Attribute msg)
    , listItemAttributes : List (Attribute msg)
    }


type Dropdown msg
    = Dropdown (Options msg)


config : msg -> (String -> msg) -> Dropdown msg
config clickedMsg itemPickedMsg =
    Dropdown
        { promptText = "-- Select --"
        , clickedMsg = clickedMsg
        , itemPickedMsg = itemPickedMsg
        , containerAttributes = []
        , disabledAttributes = []
        , inputAttributes = []
        , textAttributes = []
        , listAttributes = []
        , listItemAttributes = []
        }


withPromptText : String -> Dropdown msg -> Dropdown msg
withPromptText promptText (Dropdown options) =
    Dropdown { options | promptText = promptText }


withContainerAttributes : List (Attribute msg) -> Dropdown msg -> Dropdown msg
withContainerAttributes attrs (Dropdown options) =
    Dropdown { options | containerAttributes = attrs }


withDisabledAttributes : List (Attribute msg) -> Dropdown msg -> Dropdown msg
withDisabledAttributes attrs (Dropdown options) =
    Dropdown { options | disabledAttributes = attrs }


withInputAttributes : List (Attribute msg) -> Dropdown msg -> Dropdown msg
withInputAttributes attrs (Dropdown options) =
    Dropdown { options | inputAttributes = attrs }


withTextAttributes : List (Attribute msg) -> Dropdown msg -> Dropdown msg
withTextAttributes attrs (Dropdown options) =
    Dropdown { options | textAttributes = attrs }


withListAttributes : List (Attribute msg) -> Dropdown msg -> Dropdown msg
withListAttributes attrs (Dropdown options) =
    Dropdown { options | listAttributes = attrs }


withListItemAttributes : List (Attribute msg) -> Dropdown msg -> Dropdown msg
withListItemAttributes attrs (Dropdown options) =
    Dropdown { options | listItemAttributes = attrs }


toEl : State -> List String -> Dropdown msg -> Element msg
toEl state data (Dropdown options) =
    let
        mainText =
            state.selectedItem
                |> Maybe.withDefault options.promptText

        items =
            if state.isOpen then
                column options.listAttributes (List.map (viewItem options) data)

            else
                none

        mainAttr =
            case data of
                [] ->
                    options.disabledAttributes ++ options.inputAttributes

                _ ->
                    onClick options.clickedMsg :: options.inputAttributes
    in
    column
        options.containerAttributes
        [ row
            mainAttr
            [ el options.textAttributes (text mainText)
            , el [] (text "▾")
            ]
        , el [ width fill, inFront items ] none
        ]


viewItem : Options msg -> String -> Element msg
viewItem options item =
    let
        attrs =
            (onClick <| options.itemPickedMsg item)
                :: options.listItemAttributes
    in
    el attrs (text item)



-- helper to cancel click anywhere


onClick : msg -> Attribute msg
onClick message =
    Events.onClick message
