module Dropdown exposing (Config, Msg, State, basic, filterable, init, update, view, withContainerAttributes, withDisabledAttributes, withHeadAttributes, withItemToText, withListAttributes, withOpenCloseButtons, withPromptElement, withSearchAttributes, withTextAttributes)

import Element exposing (..)
import Element.Events as Events
import Element.Input as Input
import Html.Events
import Json.Decode as Decode
import Task


type DropdownType
    = Basic
    | Filterable


type State item
    = State
        { selectedItem : Maybe item
        , isOpen : Bool
        , filterText : String
        , focusedIndex : Int
        }


type Config item msg
    = Config
        { dropdownType : DropdownType
        , promptElement : Element msg
        , filterPlaceholder : String
        , dropdownMsg : Msg item -> msg
        , onSelectMsg : Maybe item -> msg
        , containerAttributes : List (Attribute msg)
        , disabledAttributes : List (Attribute msg)
        , headAttributes : List (Attribute msg)
        , searchAttributes : List (Attribute msg)
        , textAttributes : List (Attribute msg)
        , listAttributes : List (Attribute msg)
        , itemToElement : Bool -> item -> Element msg
        , openButton : Element msg
        , closeButton : Element msg
        , itemToText : item -> String
        }


type Msg item
    = NoOp
    | OnBlur
    | OnClear
    | OnClickPrompt
    | OnEsc
    | OnSelect item
    | OnFilterTyped String
    | OnKeyDown Key


type Key
    = Other
    | ArrowDown
    | ArrowUp
    | Enter
    | Esc
    | Space


init : State item
init =
    State
        { selectedItem = Nothing
        , isOpen = False
        , filterText = ""
        , focusedIndex = 0
        }


basic : (Msg item -> msg) -> (Maybe item -> msg) -> (Bool -> item -> Element msg) -> Config item msg
basic dropdownMsg onSelectMsg itemToElement =
    Config
        { dropdownType = Basic
        , promptElement = text "-- Select --"
        , filterPlaceholder = "Filter values"
        , dropdownMsg = dropdownMsg
        , onSelectMsg = onSelectMsg
        , containerAttributes = []
        , disabledAttributes = []
        , headAttributes = []
        , searchAttributes = []
        , textAttributes = []
        , listAttributes = []
        , itemToElement = itemToElement
        , openButton = text "▼"
        , closeButton = text "▲"
        , itemToText = \_ -> ""
        }


filterable : (Msg item -> msg) -> (Maybe item -> msg) -> (Bool -> item -> Element msg) -> (item -> String) -> Config item msg
filterable dropdownMsg onSelectMsg itemToElement itemToText =
    Config
        { dropdownType = Filterable
        , promptElement = text "-- Select --"
        , filterPlaceholder = "Filter values"
        , dropdownMsg = dropdownMsg
        , onSelectMsg = onSelectMsg
        , containerAttributes = []
        , disabledAttributes = []
        , headAttributes = []
        , searchAttributes = []
        , textAttributes = []
        , listAttributes = []
        , itemToElement = itemToElement
        , openButton = text "▼"
        , closeButton = text "▲"
        , itemToText = itemToText
        }


withPromptElement : Element msg -> Config item msg -> Config item msg
withPromptElement promptElement (Config config) =
    Config { config | promptElement = promptElement }


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


withItemToText : (item -> String) -> Config item msg -> Config item msg
withItemToText itemToText (Config config) =
    Config { config | itemToText = itemToText }


withOpenCloseButtons : { openButton : Element msg, closeButton : Element msg } -> Config item msg -> Config item msg
withOpenCloseButtons { openButton, closeButton } (Config config) =
    Config { config | openButton = openButton, closeButton = closeButton }



-- Update


update : Config item msg -> Msg item -> State item -> List item -> ( State item, Cmd msg )
update (Config config) msg (State state) data =
    let
        _ =
            Debug.log "msg" msg

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
                                |> Task.perform config.onSelectMsg
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
                                |> Task.perform config.onSelectMsg
                    in
                    ( { state | isOpen = False, selectedItem = Just item }, cmd )

                OnFilterTyped val ->
                    ( { state | filterText = val }, Cmd.none )

                OnKeyDown key ->
                    let
                        newIndex =
                            case key of
                                ArrowUp ->
                                    if state.focusedIndex == 0 then
                                        List.length data - 1

                                    else
                                        state.focusedIndex - 1

                                ArrowDown ->
                                    if state.focusedIndex >= List.length data - 1 then
                                        0

                                    else
                                        state.focusedIndex + 1

                                _ ->
                                    state.focusedIndex

                        isOpen =
                            case key of
                                Esc ->
                                    False

                                Enter ->
                                    False

                                _ ->
                                    True

                        focusedItem =
                            data
                                |> List.indexedMap (\i item -> ( i, item ))
                                |> List.filter (\( i, _ ) -> i == state.focusedIndex)
                                |> List.head
                                |> Maybe.map Tuple.second

                        cmd =
                            case key of
                                Enter ->
                                    Task.succeed focusedItem
                                        |> Task.perform config.onSelectMsg

                                _ ->
                                    Cmd.none
                    in
                    ( { state | focusedIndex = newIndex, isOpen = isOpen }, cmd )
    in
    ( State newState, newCommand )



-- View


view : Config item msg -> State item -> List item -> Element msg
view (Config config) (State state) data =
    let
        onClickMsg =
            onClick (config.dropdownMsg OnClickPrompt)

        promptElement =
            state.selectedItem
                |> Maybe.map (config.itemToElement False)
                |> Maybe.withDefault config.promptElement

        prompt =
            el (onClickMsg :: config.textAttributes) promptElement

        filteredData =
            data
                |> List.filter (\i -> String.contains state.filterText (config.itemToText i))

        headAttrs =
            case filteredData of
                [] ->
                    config.headAttributes ++ config.disabledAttributes

                _ ->
                    config.headAttributes

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
                    itemView i item =
                        let
                            highlighed =
                                i == state.focusedIndex
                        in
                        el
                            [ onClick <| config.dropdownMsg (OnSelect item)
                            , width fill
                            ]
                            (config.itemToElement highlighed item)

                    items =
                        column config.listAttributes (List.indexedMap itemView filteredData)
                in
                ( search, openCloseButton config.closeButton, el [ width fill, inFront items ] none )

            else
                ( prompt, openCloseButton config.openButton, none )
    in
    column
        (onKeyDown (config.dropdownMsg << OnKeyDown) :: config.containerAttributes)
        [ row headAttrs [ head, button ]
        , body
        ]



-- helper to cancel click anywhere


onClick : msg -> Attribute msg
onClick message =
    Events.onClick message


onKeyDown : (Key -> msg) -> Attribute msg
onKeyDown msg =
    let
        stringToKey str =
            case str of
                "ArrowDown" ->
                    Decode.succeed ArrowDown

                "ArrowUp" ->
                    Decode.succeed ArrowUp

                "Enter" ->
                    Decode.succeed Enter

                "Escape" ->
                    Decode.succeed Esc

                _ ->
                    Decode.fail "not used key"

        keyDecoder =
            Decode.field "key" Decode.string
                |> Decode.andThen stringToKey
    in
    htmlAttribute (Html.Events.on "keydown" (Decode.map msg keyDecoder))
