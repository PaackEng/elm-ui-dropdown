module Dropdown exposing (Dropdown, State, config, toEl, withContainerAttributes, withDisabledAttributes, withInputAttributes, withListAttributes, withListItemAttributes, withPromptText, withSearchAttributes, withTextAttributes)

import Element exposing (..)
import Element.Events as Events
import Element.Input as Input


type alias State =
    { selectedItem : Maybe String
    , isOpen : Bool
    , filterText : String
    }


type alias Options msg =
    { promptText : String
    , filterPlaceholder : String
    , clickedMsg : msg
    , searchMsg : String -> msg
    , itemPickedMsg : String -> msg
    , containerAttributes : List (Attribute msg)
    , disabledAttributes : List (Attribute msg)
    , inputAttributes : List (Attribute msg)
    , searchAttributes : List (Attribute msg)
    , textAttributes : List (Attribute msg)
    , listAttributes : List (Attribute msg)
    , listItemAttributes : List (Attribute msg)
    }


type Dropdown msg
    = Dropdown (Options msg)


config : msg -> (String -> msg) -> (String -> msg) -> Dropdown msg
config clickedMsg searchMsg itemPickedMsg =
    Dropdown
        { promptText = "-- Select --"
        , filterPlaceholder = "Filter values"
        , clickedMsg = clickedMsg
        , searchMsg = searchMsg
        , itemPickedMsg = itemPickedMsg
        , containerAttributes = []
        , disabledAttributes = []
        , inputAttributes = []
        , searchAttributes = []
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


withSearchAttributes : List (Attribute msg) -> Dropdown msg -> Dropdown msg
withSearchAttributes attrs (Dropdown options) =
    Dropdown { options | searchAttributes = attrs }


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

        headAttrs =
            case data of
                [] ->
                    options.disabledAttributes ++ options.inputAttributes

                _ ->
                    options.inputAttributes

        prompt =
            el (onClick options.clickedMsg :: options.textAttributes) (text mainText)

        search =
            Input.search options.searchAttributes
                { onChange = options.searchMsg
                , text = state.filterText
                , placeholder = Just <| Input.placeholder [] (text options.filterPlaceholder)
                , label = Input.labelHidden "Filter List"
                }

        ( head, body ) =
            if state.isOpen then
                let
                    items =
                        column options.listAttributes (List.map (viewItem options) data)
                in
                ( search, el [ width fill, inFront items ] none )

            else
                ( prompt, none )
    in
    column
        options.containerAttributes
        [ row headAttrs [ head, el [ onClick options.clickedMsg ] (text "▾") ]
        , body
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
