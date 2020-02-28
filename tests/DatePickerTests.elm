module DatePickerTests exposing (suite)

import Date
import DatePicker
import Element exposing (Element)
import Element.Input as Input
import Expect exposing (Expectation)
import Fuzz exposing (intRange)
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
        , initSetTodayIsInitWithToday
        , visibleMonthIsVisible
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
                            model
                                |> DatePicker.update msg
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


initSetTodayIsInitWithToday : Test
initSetTodayIsInitWithToday =
    fuzz2 (intRange 1000 3000) (intRange 1 366) "init + setToday = initWithToday" <|
        \year day ->
            let
                date =
                    Date.fromOrdinalDate year day

                setToday =
                    DatePicker.init |> DatePicker.setToday date

                withToday =
                    DatePicker.initWithToday date
            in
            Expect.equal setToday withToday


visibleMonthIsVisible : Test
visibleMonthIsVisible =
    fuzz2 (intRange 1000 3000) (intRange 1 366) "today is visible month" <|
        \year day ->
            let
                date =
                    Date.fromOrdinalDate year day

                model =
                    DatePicker.initWithToday date
                        |> DatePicker.open
            in
            modelToSingle model
                |> findCalendar
                |> Query.has [ Selector.text <| Date.format "MMMM yyyy" date ]



-- EXPECTATIONS


isClosed : DatePicker.Model -> Expectation
isClosed model =
    modelToSingle model
        |> Query.hasNot [ Selector.attribute TestHelper.calendarAttrHtml ]


isOpen : DatePicker.Model -> Expectation
isOpen model =
    modelToSingle model
        |> Query.has [ Selector.attribute TestHelper.calendarAttrHtml ]



-- HELPERS


findCalendar : Query.Single Msg -> Query.Single Msg
findCalendar =
    Query.find [ Selector.attribute TestHelper.calendarAttrHtml ]


type Msg
    = DatePickerChanged DatePicker.ChangeEvent


modelToSingle : DatePicker.Model -> Query.Single Msg
modelToSingle model =
    simplePicker model
        |> toHtml
        |> Query.fromHtml


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
