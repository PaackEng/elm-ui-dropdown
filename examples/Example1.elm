module Example1 exposing (..)

import Browser
import Dropdown
import Element exposing (..)
import Html exposing (Html)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { country : Maybe String
    , isOpen : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { country = Nothing
      , isOpen = False
      }
    , Cmd.none
    )


countries : List String
countries =
    [ "Option 1", "Option 2", "Option 3" ]


countryConfig : Dropdown.Config Msg
countryConfig =
    { defaultText = "-- pick a country --"
    , clickedMsg = ToggleDropdown
    , itemPickedMsg = CountryPicked
    , containerAttributes = []
    , disabledAttributes = []
    , inputAttributes = []
    , textAttributes = []
    , listAttributes = []
    , listItemAttributes = []
    }



-- UPDATE


type Msg
    = ToggleDropdown
    | CountryPicked String
    | Blur


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleDropdown ->
            ( { model | isOpen = not model.isOpen }, Cmd.none )

        CountryPicked pickedCountry ->
            ( { model
                | country = Just pickedCountry
                , isOpen = False
              }
            , Cmd.none
            )

        Blur ->
            ( { model | isOpen = False }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        countryContext =
            { selectedItem = model.country
            , isOpen = model.isOpen
            }
    in
    el []
        (Dropdown.view countryConfig countryContext countries)
        |> layout []
