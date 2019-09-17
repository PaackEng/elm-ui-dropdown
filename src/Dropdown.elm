module Dropdown exposing (Config, Msg, State, basic, filterable, init, update, view, withContainerAttributes, withDisabledAttributes, withHeadAttributes, withItemToElement, withItemToPrompt, withListAttributes, withOpenCloseButtons, withPromptText, withSearchAttributes, withTextAttributes)

import Element exposing (..)
import Element.Events as Events
import Element.Input as Input
import Task


type DropdownType
    = Basic
    | Filterable


type State item
    = State
        { selectedItem : Maybe item
        , isOpen : Bool
        , filterText : String
        }


type Config item msg
    = Config
        { dropdownType : DropdownType
        , promptText : String
        , filterPlaceholder : String
        , dropdownMsg : Msg item -> msg
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
    | OnFilterTyped String


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


basic : (Msg item -> msg) -> (Maybe item -> msg) -> Config item msg
basic dropdownMsg itemPickedMsg =
    Config
        { dropdownType = Basic
        , promptText = "-- Select --"
        , filterPlaceholder = "Filter values"
        , dropdownMsg = dropdownMsg
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


filterable : (Msg item -> msg) -> (Maybe item -> msg) -> Config item msg
filterable dropdownMsg itemPickedMsg =
    Config
        { dropdownType = Filterable
        , promptText = "-- Select --"
        , filterPlaceholder = "Filter values"
        , dropdownMsg = dropdownMsg
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


withItemToPrompt : (item -> String) -> Config item msg -> Config item msg
withItemToPrompt itemToPrompt (Config config) =
    Config { config | itemToPrompt = itemToPrompt }


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

                OnFilterTyped val ->
                    ( { state | filterText = val }, Cmd.none )
    in
    ( State newState, newCommand )



-- View


view : Config item msg -> State item -> List item -> Element msg
view (Config config) (State state) data =
    let
        onClickMsg =
            onClick (config.dropdownMsg OnClickPrompt)

        mainText =
            state.selectedItem
                |> Maybe.map config.itemToPrompt
                |> Maybe.withDefault config.promptText

        filteredData =
            data
                |> List.filter (\i -> String.contains state.filterText (config.itemToPrompt i))

        headAttrs =
            case filteredData of
                [] ->
                    config.headAttributes ++ config.disabledAttributes

                _ ->
                    config.headAttributes

        prompt =
            el (onClickMsg :: config.textAttributes) (text mainText)

        search =
            case config.dropdownType of
                Basic ->
                    prompt

                Filterable ->
                    Input.search config.searchAttributes
                        { onChange = config.dropdownMsg << OnFilterTyped
                        , text = state.filterText
                        , placeholder = Just <| Input.placeholder [] (text config.filterPlaceholder)
                        , label = Input.labelHidden "Filter List"
                        }

        openCloseButton b =
            case filteredData of
                [] ->
                    el [] b

                _ ->
                    el [ onClickMsg ] b

        ( head, button, body ) =
            if state.isOpen then
                let
                    itemView item =
                        el
                            [ onClick <| config.dropdownMsg (OnSelect item)
                            , width fill
                            ]
                            (config.itemToElement item)

                    items =
                        column config.listAttributes (List.map itemView filteredData)
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
