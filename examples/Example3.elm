module Example3 exposing (..)

import Browser
import Dict exposing (Dict)
import Dropdown
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Html exposing (Html)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { countryDropdownState : Dropdown.State String
    , cityDropdownState : Dropdown.State String
    , country : Maybe Country
    , city : Maybe City
    , openDropDown : OpenDropDown
    }


type OpenDropDown
    = AllClosed
    | CountryDropDown
    | CityDropDown


init : () -> ( Model, Cmd Msg )
init _ =
    ( { countryDropdownState = Dropdown.init "country-dropdown"
      , cityDropdownState = Dropdown.init "city-dropdown"
      , country = Nothing
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
    | CountryPicked (Maybe Country)
    | CityPicked (Maybe City)
    | CountryDropdownMsg (Dropdown.Msg String)
    | CityDropdownMsg (Dropdown.Msg String)


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

        CountryPicked country ->
            let
                newCity =
                    if model.country /= country then
                        Nothing

                    else
                        model.city
            in
            ( { model
                | country = country
                , city = newCity
                , openDropDown = AllClosed
              }
            , Cmd.none
            )

        CityPicked city ->
            ( { model
                | city = city
                , openDropDown = AllClosed
              }
            , Cmd.none
            )

        CountryDropdownMsg subMsg ->
            let
                ( state, cmd ) =
                    Dropdown.update countryConfig subMsg model.countryDropdownState countries
            in
            ( { model | countryDropdownState = state }, cmd )

        CityDropdownMsg subMsg ->
            let
                cities =
                    model.country
                        |> Maybe.andThen (\c -> Dict.get c allCities)
                        |> Maybe.withDefault []

                ( state, cmd ) =
                    Dropdown.update cityConfig subMsg model.cityDropdownState cities
            in
            ( { model | cityDropdownState = state }, cmd )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        cities =
            model.country
                |> Maybe.andThen (\c -> Dict.get c allCities)
                |> Maybe.withDefault []
    in
    row []
        [ Dropdown.view countryConfig model.countryDropdownState countries
        , Dropdown.view cityConfig model.cityDropdownState cities
        ]
        |> el [ width fill, height fill, padding 20 ]
        |> layout []


countryConfig : Dropdown.Config String Msg
countryConfig =
    dropdownConfig CountryDropdownMsg CountryPicked


cityConfig : Dropdown.Config String Msg
cityConfig =
    dropdownConfig CityDropdownMsg CityPicked


dropdownConfig : (Dropdown.Msg String -> Msg) -> (Maybe String -> Msg) -> Dropdown.Config String Msg
dropdownConfig dropdownMsg itemPickedMsg =
    let
        containerAttrs =
            [ width (px 300) ]

        selectAttrs =
            [ Border.width 1, Border.rounded 5, paddingXY 16 8, spacing 10, width fill ]

        searchAttrs =
            [ Border.width 0, padding 0 ]

        listAttrs =
            [ Border.width 1
            , Border.roundEach { topLeft = 0, topRight = 0, bottomLeft = 5, bottomRight = 5 }
            , width fill
            , spacing 5
            ]

        itemToPrompt item =
            text item

        itemToElement selected highlighted i =
            let
                bgColor =
                    if highlighted then
                        rgb255 128 128 128

                    else if selected then
                        rgb255 100 100 100

                    else
                        rgb255 255 255 255
            in
            el
                [ Background.color bgColor
                , padding 8
                , spacing 10
                , width fill
                ]
                (text i)
    in
    Dropdown.filterable dropdownMsg itemPickedMsg itemToPrompt itemToElement identity
        |> Dropdown.withContainerAttributes containerAttrs
        |> Dropdown.withSelectAttributes selectAttrs
        |> Dropdown.withListAttributes listAttrs
        |> Dropdown.withSearchAttributes searchAttrs
