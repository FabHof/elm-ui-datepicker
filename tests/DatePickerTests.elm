module DatePickerTests exposing (suite)

import Date exposing (Date)
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
        , close
        , focusOpen
        , blurClose
        , initSetTodayIsInitWithToday
        , todayIsVisibleOnInit
        , setVisibleIsVisible
        , nextMonth
        , previousMonth
        , selectedDay
        , clickDay
        , clickDisabled
        , setTodayDoesNotOverrideVisibleMonthIfSet
        , clickOnHeaderShowsMonthSelection
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


close : Test
close =
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


setTodayDoesNotOverrideVisibleMonthIfSet : Test
setTodayDoesNotOverrideVisibleMonthIfSet =
    fuzz2 (intRange 1000 3000) (intRange 1 334) "setToday does not override visibleMonth" <|
        \year day ->
            let
                date =
                    Date.fromOrdinalDate year day

                visibleMonth =
                    Date.fromOrdinalDate year 336

                model =
                    DatePicker.init
                        |> DatePicker.setVisibleMonth visibleMonth
                        |> DatePicker.setToday date
                        |> DatePicker.open
            in
            isVisibleMonth visibleMonth model


todayIsVisibleOnInit : Test
todayIsVisibleOnInit =
    fuzz2 (intRange 1000 3000) (intRange 1 366) "today is visible month" <|
        \year day ->
            let
                date =
                    Date.fromOrdinalDate year day

                model =
                    DatePicker.initWithToday date
                        |> DatePicker.open
            in
            model
                |> Expect.all
                    [ isVisibleMonth date
                    , todayIsThere date
                    ]


setVisibleIsVisible : Test
setVisibleIsVisible =
    fuzz2 (intRange 1000 3000) (intRange 1 366) "setVisibleMonth sets visible month" <|
        \year day ->
            let
                date =
                    Date.fromOrdinalDate year day

                model =
                    DatePicker.init
                        |> DatePicker.open
                        |> DatePicker.setVisibleMonth date
            in
            model
                |> isVisibleMonth date


nextMonth : Test
nextMonth =
    fuzz2 (intRange 1000 3000) (intRange 1 366) "Next month changes visible month" <|
        \year day ->
            let
                date =
                    Date.fromOrdinalDate year day

                model =
                    DatePicker.initWithToday date
                        |> DatePicker.open

                clickResult =
                    clickNextMonth model
            in
            case clickResult of
                Err err ->
                    Expect.fail err

                Ok (DatePickerChanged (DatePicker.PickerChanged msg)) ->
                    model
                        |> DatePicker.update msg
                        |> isVisibleMonth (date |> Date.add Date.Months 1)

                _ ->
                    Expect.fail "focus resulted in wrong changedEvent"


previousMonth : Test
previousMonth =
    fuzz2 (intRange 1000 3000) (intRange 1 366) "Previous month changes visible month" <|
        \year day ->
            let
                date =
                    Date.fromOrdinalDate year day

                model =
                    DatePicker.initWithToday date
                        |> DatePicker.open

                clickResult =
                    clickPreviousMonth model
            in
            case clickResult of
                Err err ->
                    Expect.fail err

                Ok (DatePickerChanged (DatePicker.PickerChanged msg)) ->
                    model
                        |> DatePicker.update msg
                        |> isVisibleMonth (date |> Date.add Date.Months -1)

                _ ->
                    Expect.fail "focus resulted in wrong changedEvent"


clickDay : Test
clickDay =
    fuzz3 (intRange 1000 3000) (intRange 1 366) (intRange 1 31) "Clicking a date has correct event" <|
        \year day select ->
            let
                date =
                    Date.fromOrdinalDate year day

                lastDayInMonth =
                    Date.fromCalendarDate (Date.year date) (Date.month date) 31

                model =
                    DatePicker.initWithToday date
                        |> DatePicker.open

                dayToSelect =
                    min select (Date.day lastDayInMonth)
            in
            modelToSingle model
                |> findTable
                |> Query.find
                    [ Selector.attribute TestHelper.dayInMonthAttrHtml
                    , Selector.containing
                        [ Selector.text <|
                            intTo2DigitString dayToSelect
                        ]
                    ]
                |> Event.simulate Event.click
                |> Event.expect
                    (DatePickerChanged <|
                        DatePicker.DateChanged <|
                            Date.fromCalendarDate (Date.year date) (Date.month date) dayToSelect
                    )


clickDisabled : Test
clickDisabled =
    fuzz3 (intRange 1000 3000) (intRange 1 366) (intRange 1 31) "Clicking a disabled date does not fire an event" <|
        \year day select ->
            let
                date =
                    Date.fromOrdinalDate year day

                lastDayInMonth =
                    Date.fromCalendarDate (Date.year date) (Date.month date) 31

                model =
                    DatePicker.initWithToday date
                        |> DatePicker.open

                dayToSelect =
                    min select (Date.day lastDayInMonth)

                defaultSettings =
                    DatePicker.defaultSettings

                settings =
                    { defaultSettings | disabled = always True }

                simpleConfig =
                    simplePickerConfig model

                config =
                    { simpleConfig | settings = settings }
            in
            DatePicker.input [] config
                |> toHtml
                |> Query.fromHtml
                |> findTable
                |> Query.find
                    [ Selector.attribute TestHelper.dayInMonthAttrHtml
                    , Selector.containing
                        [ Selector.text <|
                            intTo2DigitString dayToSelect
                        ]
                    ]
                |> Event.simulate Event.click
                |> Event.toResult
                |> Expect.err


intTo2DigitString : Int -> String
intTo2DigitString num =
    if num < 10 then
        "0" ++ String.fromInt num

    else
        String.fromInt num


selectedDay : Test
selectedDay =
    test "selected day is there" <|
        \_ ->
            let
                model =
                    DatePicker.initWithToday (Date.fromOrdinalDate 2020 1)
                        |> DatePicker.open

                simpleConfig =
                    simplePickerConfig model

                selectedConfig =
                    { simpleConfig | selected = Just <| Date.fromOrdinalDate 2020 1 }
            in
            DatePicker.input [] selectedConfig
                |> toHtml
                |> Query.fromHtml
                |> Query.has [ Selector.attribute TestHelper.selectedAttrHtml ]


clickOnHeaderShowsMonthSelection : Test
clickOnHeaderShowsMonthSelection =
    test "clicking header shows month selection" <|
        \_ ->
            let
                model =
                    DatePicker.initWithToday (Date.fromOrdinalDate 2020 1)
                        |> DatePicker.open

                clickResult =
                    modelToSingle model
                        |> findCalendar
                        |> Query.findAll [ Selector.tag "div", Selector.containing [ Selector.text "January 2020" ] ]
                        |> (Query.keep <| Selector.containing [ Selector.text "January 2020" ])
                        |> (Query.keep <| Selector.tag "div")
                        |> Query.first
                        |> Event.simulate Event.click
                        |> Event.toResult
            in
            case clickResult of
                Err err ->
                    Expect.fail err

                Ok (DatePickerChanged (DatePicker.PickerChanged msg)) ->
                    model
                        |> DatePicker.update msg
                        |> isInMonthView "2020"

                _ ->
                    Expect.fail "focus resulted in wrong changedEvent"



-- EXPECTATIONS


isInMonthView : String -> DatePicker.Model -> Expectation
isInMonthView year model =
    modelToSingle model
        |> Expect.all
            (Query.has [ Selector.text year ]
                :: (List.range 1 12
                        |> List.map (\m -> Date.numberToMonth m)
                        |> List.map (\m -> Date.fromCalendarDate 2020 m 1)
                        |> List.map (\d -> Date.format "MMM" d)
                        |> List.map (\m -> Query.has [ Selector.text m ])
                   )
             -- [ Query.has [ Selector.text "Jan" ]
             --    , Query.has [ Selector.text "Feb" ]
             --    ]
            )


isVisibleMonth : Date -> DatePicker.Model -> Expectation
isVisibleMonth date model =
    modelToSingle model
        |> findCalendar
        |> Query.has [ Selector.text <| Date.format "MMMM yyyy" date ]


todayIsThere : Date -> DatePicker.Model -> Expectation
todayIsThere today model =
    modelToSingle model
        |> Query.find [ Selector.attribute TestHelper.todayAttrHtml ]
        |> Query.has [ Selector.text (Date.day today |> String.fromInt) ]


isClosed : DatePicker.Model -> Expectation
isClosed model =
    modelToSingle model
        |> Query.hasNot [ Selector.attribute TestHelper.calendarAttrHtml ]


isOpen : DatePicker.Model -> Expectation
isOpen model =
    modelToSingle model
        |> Query.has [ Selector.attribute TestHelper.calendarAttrHtml ]



-- HELPERS


type Msg
    = DatePickerChanged DatePicker.ChangeEvent


findCalendar : Query.Single Msg -> Query.Single Msg
findCalendar =
    Query.find [ Selector.attribute TestHelper.calendarAttrHtml ]


findTable : Query.Single Msg -> Query.Single Msg
findTable =
    Query.find [ Selector.attribute TestHelper.tableAttrHtml ]


findNextMonthButton : Query.Single Msg -> Query.Single Msg
findNextMonthButton =
    Query.find [ Selector.attribute TestHelper.nextMonthAttrHtml ]


findPreviousMonthButton : Query.Single Msg -> Query.Single Msg
findPreviousMonthButton =
    Query.find [ Selector.attribute TestHelper.previousMonthAttrHtml ]


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
    DatePicker.input [] (simplePickerConfig model)


simplePickerConfig :
    DatePicker.Model
    ->
        { onChange : DatePicker.ChangeEvent -> Msg
        , selected : Maybe Date
        , text : String
        , label : Input.Label msg
        , placeholder : Maybe (Input.Placeholder msg)
        , model : DatePicker.Model
        , settings : DatePicker.Settings
        }
simplePickerConfig model =
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
    modelToSingle model
        |> Query.find [ Selector.attribute TestHelper.inputAttrHtml ]
        |> Event.simulate event
        |> Event.toResult


clickNextMonth : DatePicker.Model -> Result String Msg
clickNextMonth model =
    modelToSingle model
        |> findNextMonthButton
        |> Event.simulate Event.click
        |> Event.toResult


clickPreviousMonth : DatePicker.Model -> Result String Msg
clickPreviousMonth model =
    modelToSingle model
        |> findPreviousMonthButton
        |> Event.simulate Event.click
        |> Event.toResult
