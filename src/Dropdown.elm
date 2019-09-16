module Dropdown exposing (Dropdown, State, basic, filterable, view, withContainerAttributes, withDisabledAttributes, withHeadAttributes, withItemToElement, withListAttributes, withOpenCloseButtons, withPromptText, withSearchAttributes, withTextAttributes)

import Element exposing (..)
import Element.Events as Events
import Element.Input as Input


type alias State =
    { selectedItem : Maybe String
    , isOpen : Bool
    , filterText : String
    }


type alias Options item msg =
    { promptText : String
    , filterPlaceholder : String
    , clickedMsg : msg
    , searchMsg : Maybe (String -> msg)
    , itemPickedMsg : item -> msg
    , containerAttributes : List (Attribute msg)
    , disabledAttributes : List (Attribute msg)
    , headAttributes : List (Attribute msg)
    , searchAttributes : List (Attribute msg)
    , textAttributes : List (Attribute msg)
    , listAttributes : List (Attribute msg)
    , itemToElement : item -> Element msg
    , openButton : Element msg
    , closeButton : Element msg
    }


type Dropdown item msg
    = Dropdown (Options item msg)


basic : msg -> (item -> msg) -> Dropdown item msg
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
        , itemToElement = \_ -> none
        , openButton = text "▼"
        , closeButton = text "▲"
        }


filterable : msg -> (String -> msg) -> (item -> msg) -> Dropdown item msg
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
        , itemToElement = \_ -> none
        , openButton = text "▼"
        , closeButton = text "▲"
        }


withPromptText : String -> Dropdown item msg -> Dropdown item msg
withPromptText promptText (Dropdown options) =
    Dropdown { options | promptText = promptText }


withContainerAttributes : List (Attribute msg) -> Dropdown item msg -> Dropdown item msg
withContainerAttributes attrs (Dropdown options) =
    Dropdown { options | containerAttributes = attrs }


withDisabledAttributes : List (Attribute msg) -> Dropdown item msg -> Dropdown item msg
withDisabledAttributes attrs (Dropdown options) =
    Dropdown { options | disabledAttributes = attrs }


withHeadAttributes : List (Attribute msg) -> Dropdown item msg -> Dropdown item msg
withHeadAttributes attrs (Dropdown options) =
    Dropdown { options | headAttributes = attrs }


withSearchAttributes : List (Attribute msg) -> Dropdown item msg -> Dropdown item msg
withSearchAttributes attrs (Dropdown options) =
    Dropdown { options | searchAttributes = attrs }


withTextAttributes : List (Attribute msg) -> Dropdown item msg -> Dropdown item msg
withTextAttributes attrs (Dropdown options) =
    Dropdown { options | textAttributes = attrs }


withListAttributes : List (Attribute msg) -> Dropdown item msg -> Dropdown item msg
withListAttributes attrs (Dropdown options) =
    Dropdown { options | listAttributes = attrs }


withItemToElement : (item -> Element msg) -> Dropdown item msg -> Dropdown item msg
withItemToElement itemToElement (Dropdown options) =
    Dropdown { options | itemToElement = itemToElement }


withOpenCloseButtons : { openButton : Element msg, closeButton : Element msg } -> Dropdown item msg -> Dropdown item msg
withOpenCloseButtons { openButton, closeButton } (Dropdown options) =
    Dropdown { options | openButton = openButton, closeButton = closeButton }



-- View


view : Dropdown item msg -> State -> List item -> Element msg
view (Dropdown options) state data =
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

        openCloseButton b =
            case data of
                [] ->
                    el [] b

                _ ->
                    el [ onClick options.clickedMsg ] b

        ( head, button, body ) =
            if state.isOpen then
                let
                    items =
                        column options.listAttributes (List.map (itemView options) data)
                in
                ( search, openCloseButton options.closeButton, el [ width fill, inFront items ] none )

            else
                ( prompt, openCloseButton options.openButton, none )
    in
    column
        options.containerAttributes
        [ row headAttrs [ head, button ]
        , body
        ]


itemView : Options item msg -> item -> Element msg
itemView { itemPickedMsg, itemToElement } item =
    el [ onClick (itemPickedMsg item), width fill ] (itemToElement item)



-- helper to cancel click anywhere


onClick : msg -> Attribute msg
onClick message =
    Events.onClick message
