module DatePickerTests exposing (suite)

import DatePicker
import Element exposing (Element)
import Element.Input as Input
import Expect exposing (Expectation)
import Html exposing (Html)
import Internal.TestHelper as TestHelper
import Json.Encode exposing (Value)
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector



-- TESTS


suite : Test
suite =
    describe "DatePicker"
        [ startsClosed
        , open
        , openCloseIsClosed
        , focusOpen
        , blurClose
        ]


startsClosed : Test
startsClosed =
    test "DatePicker.init is closed" <|
        \_ ->
            DatePicker.init
                |> isClosed


open : Test
open =
    test "DatePicker.open is opens the date picker" <|
        \_ ->
            DatePicker.init
                |> DatePicker.open
                |> isOpen


openCloseIsClosed : Test
openCloseIsClosed =
    test "DatePicker.close closes again" <|
        \_ ->
            DatePicker.init
                |> DatePicker.open
                |> DatePicker.close
                |> isClosed


focusOpen : Test
focusOpen =
    test "Date picker opens on focus" <|
        \_ ->
            let
                model =
                    DatePicker.init

                focusResult =
                    eventOnInputField model Event.focus
            in
            case focusResult of
                Err err ->
                    Expect.fail err

                Ok (DatePickerChanged changedEvent) ->
                    case changedEvent of
                        DatePicker.PickerChanged msg ->
                            DatePicker.update msg model
                                |> isOpen

                        _ ->
                            Expect.fail "focus resulted in wrong changedEvent"


blurClose : Test
blurClose =
    test "Date picker closes on blur" <|
        \_ ->
            let
                model =
                    DatePicker.init
                        |> DatePicker.open

                blurResult =
                    eventOnInputField model Event.blur
            in
            case blurResult of
                Err err ->
                    Expect.fail err

                Ok (DatePickerChanged changedEvent) ->
                    case changedEvent of
                        DatePicker.PickerChanged msg ->
                            DatePicker.update msg model
                                |> isClosed

                        _ ->
                            Expect.fail "focus resulted in wrong changedEvent"



-- EXPECTATIONS


isClosed : DatePicker.Model -> Expectation
isClosed model =
    model
        |> countCalendar 0


isOpen : DatePicker.Model -> Expectation
isOpen model =
    model
        |> countCalendar 1


countCalendar : Int -> DatePicker.Model -> Expectation
countCalendar count model =
    simplePicker model
        |> toHtml
        |> Query.fromHtml
        |> Query.findAll [ Selector.attribute TestHelper.calendarAttrHtml ]
        |> (Query.count <|
                Expect.equal count
           )



-- HELPERS


type Msg
    = DatePickerChanged DatePicker.ChangeEvent


toHtml : Element msg -> Html msg
toHtml =
    Element.layout []


simplePicker : DatePicker.Model -> Element Msg
simplePicker model =
    DatePicker.input []
        { onChange = DatePickerChanged
        , selected = Nothing
        , text = ""
        , label = Input.labelAbove [] (Element.text "date picker")
        , placeholder = Nothing
        , model = model
        , settings = DatePicker.defaultSettings
        }


eventOnInputField : DatePicker.Model -> ( String, Value ) -> Result String Msg
eventOnInputField model event =
    simplePicker model
        |> toHtml
        |> Query.fromHtml
        |> Query.find [ Selector.attribute TestHelper.inputAttrHtml ]
        |> Event.simulate event
        |> Event.toResult
