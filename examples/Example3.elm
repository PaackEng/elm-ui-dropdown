module Example3 exposing (..)

import Browser
import Dict exposing (Dict)
import Dropdown
import Element exposing (..)
import Element.Background as Background
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
    ( { countryDropdownState = Dropdown.init
      , cityDropdownState = Dropdown.init
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
                    Dropdown.update countryConfig subMsg model.countryDropdownState
            in
            ( { model | countryDropdownState = state }, cmd )

        CityDropdownMsg subMsg ->
            let
                ( state, cmd ) =
                    Dropdown.update cityConfig subMsg model.cityDropdownState
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
            el
                [ mouseOver [ Background.color (rgb255 128 128 128) ]
                , Font.size 16
                , Font.center
                , padding 8
                , width fill
                ]
                (text i)
    in
    Dropdown.filterable dropdownMsg itemPickedMsg
        |> Dropdown.withItemToText identity
        |> Dropdown.withItemToElement itemToElement
        |> Dropdown.withContainerAttributes containerAttrs
        |> Dropdown.withHeadAttributes inputAttrs
        |> Dropdown.withSearchAttributes searchAttrs
        |> Dropdown.withTextAttributes textAttrs
        |> Dropdown.withListAttributes listAttrs
