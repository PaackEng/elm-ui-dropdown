module Dropdown exposing (Dropdown, State, basic, filterable, toEl, withContainerAttributes, withDisabledAttributes, withHeadAttributes, withListAttributes, withListItemAttributes, withPromptText, withSearchAttributes, withTextAttributes)

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
    , searchMsg : Maybe (String -> msg)
    , itemPickedMsg : String -> msg
    , containerAttributes : List (Attribute msg)
    , disabledAttributes : List (Attribute msg)
    , headAttributes : List (Attribute msg)
    , searchAttributes : List (Attribute msg)
    , textAttributes : List (Attribute msg)
    , listAttributes : List (Attribute msg)
    , listItemAttributes : List (Attribute msg)
    }


type Dropdown msg
    = Dropdown (Options msg)


basic : msg -> (String -> msg) -> Dropdown msg
basic clickedMsg itemPickedMsg =
    Dropdown
        { promptText = "-- Select --"
        , filterPlaceholder = "Filter values"
        , clickedMsg = clickedMsg
        , searchMsg = Nothing
        , itemPickedMsg = itemPickedMsg
        , containerAttributes = []
        , disabledAttributes = []
        , headAttributes = []
        , searchAttributes = []
        , textAttributes = []
        , listAttributes = []
        , listItemAttributes = []
        }


filterable : msg -> (String -> msg) -> (String -> msg) -> Dropdown msg
filterable clickedMsg searchMsg itemPickedMsg =
    Dropdown
        { promptText = "-- Select --"
        , filterPlaceholder = "Filter values"
        , clickedMsg = clickedMsg
        , searchMsg = Just searchMsg
        , itemPickedMsg = itemPickedMsg
        , containerAttributes = []
        , disabledAttributes = []
        , headAttributes = []
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


withHeadAttributes : List (Attribute msg) -> Dropdown msg -> Dropdown msg
withHeadAttributes attrs (Dropdown options) =
    Dropdown { options | headAttributes = attrs }


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
                    options.headAttributes ++ options.disabledAttributes

                _ ->
                    options.headAttributes

        prompt =
            el (onClick options.clickedMsg :: options.textAttributes) (text mainText)

        search =
            case options.searchMsg of
                Just searchMsg ->
                    Input.search options.searchAttributes
                        { onChange = searchMsg
                        , text = state.filterText
                        , placeholder = Just <| Input.placeholder [] (text options.filterPlaceholder)
                        , label = Input.labelHidden "Filter List"
                        }

                Nothing ->
                    prompt

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
