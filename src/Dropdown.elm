module Dropdown exposing (Config, Msg, State, basic, filterable, init, update, view, withContainerAttributes, withDisabledAttributes, withHeadAttributes, withItemToElement, withListAttributes, withOpenCloseButtons, withPromptText, withSearchAttributes, withTextAttributes)

import Element exposing (..)
import Element.Events as Events
import Element.Input as Input
import Task


type State item
    = State
        { selectedItem : Maybe item
        , isOpen : Bool
        , filterText : String
        }


type Config item msg
    = Config
        { promptText : String
        , filterPlaceholder : String
        , clickedMsg : msg
        , searchMsg : Maybe (String -> msg)
        , itemPickedMsg : Maybe item -> msg
        , containerAttributes : List (Attribute msg)
        , disabledAttributes : List (Attribute msg)
        , headAttributes : List (Attribute msg)
        , searchAttributes : List (Attribute msg)
        , textAttributes : List (Attribute msg)
        , listAttributes : List (Attribute msg)
        , itemToElement : item -> Element msg
        , openButton : Element msg
        , closeButton : Element msg
        , itemToPrompt : item -> String
        }


type Msg item
    = NoOp
    | OnBlur
    | OnClear
    | OnClickPrompt
    | OnEsc
    | OnSelect item


type Key
    = KeyOther
    | KeyArrowDown
    | KeyArrowUp
    | KeyEnter
    | KeyEsc
    | KeySpace


init : State item
init =
    State
        { selectedItem = Nothing
        , isOpen = False
        , filterText = ""
        }


basic : msg -> (Maybe item -> msg) -> Config item msg
basic clickedMsg itemPickedMsg =
    Config
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
        , itemToPrompt = \_ -> ""
        }


filterable : msg -> (String -> msg) -> (Maybe item -> msg) -> Config item msg
filterable clickedMsg searchMsg itemPickedMsg =
    Config
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
        , itemToPrompt = \_ -> ""
        }


withPromptText : String -> Config item msg -> Config item msg
withPromptText promptText (Config config) =
    Config { config | promptText = promptText }


withContainerAttributes : List (Attribute msg) -> Config item msg -> Config item msg
withContainerAttributes attrs (Config config) =
    Config { config | containerAttributes = attrs }


withDisabledAttributes : List (Attribute msg) -> Config item msg -> Config item msg
withDisabledAttributes attrs (Config config) =
    Config { config | disabledAttributes = attrs }


withHeadAttributes : List (Attribute msg) -> Config item msg -> Config item msg
withHeadAttributes attrs (Config config) =
    Config { config | headAttributes = attrs }


withSearchAttributes : List (Attribute msg) -> Config item msg -> Config item msg
withSearchAttributes attrs (Config config) =
    Config { config | searchAttributes = attrs }


withTextAttributes : List (Attribute msg) -> Config item msg -> Config item msg
withTextAttributes attrs (Config config) =
    Config { config | textAttributes = attrs }


withListAttributes : List (Attribute msg) -> Config item msg -> Config item msg
withListAttributes attrs (Config config) =
    Config { config | listAttributes = attrs }


withItemToElement : (item -> Element msg) -> Config item msg -> Config item msg
withItemToElement itemToElement (Config config) =
    Config { config | itemToElement = itemToElement }


withOpenCloseButtons : { openButton : Element msg, closeButton : Element msg } -> Config item msg -> Config item msg
withOpenCloseButtons { openButton, closeButton } (Config config) =
    Config { config | openButton = openButton, closeButton = closeButton }



-- Update


update : Config item msg -> Msg item -> State item -> ( State item, Cmd msg )
update (Config config) msg (State state) =
    let
        ( newState, newCommand ) =
            case msg of
                NoOp ->
                    ( state, Cmd.none )

                OnBlur ->
                    ( { state | isOpen = False }, Cmd.none )

                OnClear ->
                    let
                        cmd =
                            Task.succeed Nothing
                                |> Task.perform config.itemPickedMsg
                    in
                    ( { state | isOpen = False, selectedItem = Nothing }, cmd )

                OnClickPrompt ->
                    ( { state | isOpen = not state.isOpen }, Cmd.none )

                OnEsc ->
                    ( { state | isOpen = False }, Cmd.none )

                OnSelect item ->
                    let
                        cmd =
                            Task.succeed (Just item)
                                |> Task.perform config.itemPickedMsg
                    in
                    ( { state | isOpen = False, selectedItem = Just item }, cmd )
    in
    ( State newState, newCommand )



-- View


view : Config item msg -> State item -> List item -> Element msg
view (Config config) (State state) data =
    let
        mainText =
            state.selectedItem
                |> Maybe.map config.itemToPrompt
                |> Maybe.withDefault config.promptText

        headAttrs =
            case data of
                [] ->
                    config.headAttributes ++ config.disabledAttributes

                _ ->
                    config.headAttributes

        prompt =
            el (onClick config.clickedMsg :: config.textAttributes) (text mainText)

        search =
            case config.searchMsg of
                Just searchMsg ->
                    Input.search config.searchAttributes
                        { onChange = searchMsg
                        , text = state.filterText
                        , placeholder = Just <| Input.placeholder [] (text config.filterPlaceholder)
                        , label = Input.labelHidden "Filter List"
                        }

                Nothing ->
                    prompt

        openCloseButton b =
            case data of
                [] ->
                    el [] b

                _ ->
                    el [ onClick config.clickedMsg ] b

        ( head, button, body ) =
            if state.isOpen then
                let
                    itemView item =
                        el
                            [ onClick <| config.itemPickedMsg <| Just item
                            , width fill
                            ]
                            (config.itemToElement item)

                    items =
                        column config.listAttributes (List.map itemView data)
                in
                ( search, openCloseButton config.closeButton, el [ width fill, inFront items ] none )

            else
                ( prompt, openCloseButton config.openButton, none )
    in
    column
        config.containerAttributes
        [ row headAttrs [ head, button ]
        , body
        ]



-- helper to cancel click anywhere


onClick : msg -> Attribute msg
onClick message =
    Events.onClick message
