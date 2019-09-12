module Dropdown exposing (..)

import Element exposing (..)
import Element.Events as Events


type alias Context =
    { selectedItem : Maybe String
    , isOpen : Bool
    }


type alias Config msg =
    { defaultText : String
    , clickedMsg : msg
    , itemPickedMsg : String -> msg
    , containerAttributes : List (Attribute msg)
    , disabledAttributes : List (Attribute msg)
    , inputAttributes : List (Attribute msg)
    , textAttributes : List (Attribute msg)
    , listAttributes : List (Attribute msg)
    , listItemAttributes : List (Attribute msg)
    }


view : Config msg -> Context -> List String -> Element msg
view config context data =
    let
        mainText =
            context.selectedItem
                |> Maybe.withDefault config.defaultText

        mainAttr =
            case data of
                [] ->
                    config.disabledAttributes ++ config.inputAttributes

                _ ->
                    onClick config.clickedMsg :: config.inputAttributes
    in
    column
        config.containerAttributes
        [ row
            mainAttr
            [ el config.textAttributes (text mainText)
            , el [] (text "▾")
            ]
        , if context.isOpen then
            column config.listAttributes (List.map (viewItem config) data)

          else
            none
        ]


viewItem : Config msg -> String -> Element msg
viewItem config item =
    let
        attrs =
            (onClick <| config.itemPickedMsg item)
                :: config.listItemAttributes
    in
    el attrs (text item)



-- helper to cancel click anywhere


onClick : msg -> Attribute msg
onClick message =
    Events.onClick message
