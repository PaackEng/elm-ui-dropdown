module Example3 exposing (..)

import Browser
import Dict exposing (Dict)
import Dropdown
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { country : Maybe Country
    , city : Maybe City
    , openDropDown : OpenDropDown
    }


type OpenDropDown
    = AllClosed
    | CountryDropDown
    | CityDropDown


init : () -> ( Model, Cmd Msg )
init _ =
    ( { country = Nothing
      , city = Nothing
      , openDropDown = AllClosed
      }
    , Cmd.none
    )



-- simple types so we can read the code better


type alias Country =
    String


type alias City =
    String



-- global constants/ config


allCities : Dict Country (List City)
allCities =
    Dict.fromList
        [ ( "Spain", [ "Barcelona", "Madrid", "Alicante", "Valencia" ] )
        , ( "Germany", [ "Berlin", "München", "Bonn", "Leipzig" ] )
        , ( "France", [ "Paris", "Lyon", "Marseille", "Dijon" ] )
        , ( "Italy", [ "Florence", "Rome", "Milan" ] )
        ]


countries : List Country
countries =
    Dict.keys allCities



-- UPDATE


type Msg
    = Toggle OpenDropDown
    | CountryPicked Country
    | CityPicked City


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Toggle dropdown ->
            let
                newOpenDropDown =
                    if model.openDropDown == dropdown then
                        AllClosed

                    else
                        dropdown
            in
            ( { model
                | openDropDown = newOpenDropDown
              }
            , Cmd.none
            )

        CountryPicked pickedCountry ->
            let
                newCity =
                    if model.country /= Just pickedCountry then
                        Nothing

                    else
                        model.city
            in
            ( { model
                | country = Just pickedCountry
                , city = newCity
                , openDropDown = AllClosed
              }
            , Cmd.none
            )

        CityPicked pickedCity ->
            ( { model
                | city = Just pickedCity
                , openDropDown = AllClosed
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        countryState =
            { selectedItem = model.country
            , isOpen = model.openDropDown == CountryDropDown
            , filterText = ""
            }

        cityState =
            { selectedItem = model.city
            , isOpen = model.openDropDown == CityDropDown
            , filterText = ""
            }

        cities =
            model.country
                |> Maybe.andThen (\c -> Dict.get c allCities)
                |> Maybe.withDefault []

        containerAttrs =
            [ width (px 300), centerX, centerY ]

        inputAttrs =
            [ Border.width 1, Border.rounded 5, paddingXY 16 8, width fill, spacing 10 ]

        searchAttrs =
            [ paddingXY 0 3 ]

        textAttrs =
            [ paddingXY 8 0, width fill ]

        listAttrs =
            [ Border.width 1
            , Border.roundEach { topLeft = 0, topRight = 0, bottomLeft = 5, bottomRight = 5 }
            , width fill
            , spacing 5
            ]

        itemToElement i =
            el [ Font.size 16, Font.center, padding 8, width fill ] (text i)

        countryConfig =
            Dropdown.basic (Toggle CountryDropDown) CountryPicked
                |> Dropdown.withPromptText "Select country"
                |> Dropdown.withItemToElement itemToElement
                |> Dropdown.withContainerAttributes containerAttrs
                |> Dropdown.withHeadAttributes inputAttrs
                |> Dropdown.withSearchAttributes searchAttrs
                |> Dropdown.withTextAttributes textAttrs
                |> Dropdown.withListAttributes listAttrs

        cityConfig =
            Dropdown.basic (Toggle CityDropDown) CityPicked
                |> Dropdown.withPromptText "Select city"
                |> Dropdown.withItemToElement itemToElement
                |> Dropdown.withContainerAttributes containerAttrs
                |> Dropdown.withHeadAttributes inputAttrs
                |> Dropdown.withSearchAttributes searchAttrs
                |> Dropdown.withTextAttributes textAttrs
                |> Dropdown.withListAttributes listAttrs
    in
    row []
        [ Dropdown.view countryConfig countryState countries
        , Dropdown.view cityConfig cityState cities
        ]
        |> el [ width fill, height fill, padding 20 ]
        |> layout []
