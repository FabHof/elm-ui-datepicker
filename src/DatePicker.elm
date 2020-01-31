module DatePicker exposing (view, DatePicker, Msg, Settings, ChangeEvent(..), defaultSettings, init, initWithToday, setToday, update)

{-|


# Main

@docs view, DatePicker, Msg, Settings, ChangeEvent, defaultSettings, init, initWithToday, setToday, update

-}

import Date exposing (Date)
import Element exposing (Attribute, Element, alignLeft, alignRight, centerX, centerY, padding, spacing)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html.Events
import Internal.Week as Week exposing (Week)
import Json.Decode as Decode
import Time exposing (Month(..), Weekday(..))


{-| Settings are the sutff you probably care about, but want to only set once for a given DatePicker.
-}
type alias Settings msg =
    { label : Input.Label msg
    , placeholder : Maybe (Input.Placeholder msg)
    , firstDayOfWeek : Weekday
    , focusColor : Element.Color
    }


{-| Handles all the internal state of the DatePicker
-}
type DatePicker
    = DatePicker Model


{-| -}
type alias Model =
    { open : Bool
    , focused : Maybe Date
    , today : Date
    }


{-| -}
init : DatePicker
init =
    DatePicker
        { open = False
        , focused = Nothing
        , today = Date.fromOrdinalDate 1 1
        }


{-| -}
initWithToday : Date -> DatePicker
initWithToday today =
    DatePicker
        { open = False
        , focused = Nothing
        , today = today
        }


{-| -}
setToday : Date -> DatePicker -> DatePicker
setToday today (DatePicker model) =
    DatePicker { model | today = today }


{-| -}
type Msg
    = ChangeFocus Date
    | OpenCalendar
    | CloseCalendar
    | NothingToDo


{-| -}
type ChangeEvent
    = DateChanged Date
    | TextChanged String
    | DateCleared
    | PickerChanged Msg


{-| -}
update : Msg -> DatePicker -> DatePicker
update msg (DatePicker model) =
    case msg of
        ChangeFocus focus ->
            DatePicker { model | focused = Just focus }

        OpenCalendar ->
            DatePicker { model | open = True }

        CloseCalendar ->
            DatePicker { model | open = False }

        NothingToDo ->
            DatePicker model


{-| Reasonable default settings. You still have to give it some input.
-}
defaultSettings : Input.Label msg -> Settings msg
defaultSettings label =
    { placeholder = Nothing
    , label = label
    , firstDayOfWeek = Mon
    , focusColor = Element.rgb255 0x00 0x7B 0xFF
    }


type alias Config msg =
    { settings : Settings msg
    , model : Model
    , text : String
    , selectedDate : Maybe Date
    , visibleMonth : Date
    , onChange : ChangeEvent -> msg
    }


{-| This view function is a wrapper arround `Input.text`. It needs the following things to work:

  - `List (Attribute Msg)` is given directly to `Input.text`.
  - `Settings Msg` is the stuff you care about, but probably want to set only once.
  - `DatePicker` is the internal model i.e. the stuff you probably do not care about.
  - `text` is given directly to `Input.text`. Yes, you have to handle user text input all by yourself.
  - `selectedDate` is only used to highlight the date when the datepicker is open. It's up to you to keep the text and selectedDate in sync.

-}
view :
    List (Attribute msg)
    ->
        { settings : Settings msg
        , datePicker : DatePicker
        , text : String
        , selectedDate : Maybe Date
        , onChange : ChangeEvent -> msg
        }
    -> Element msg
view attributes ({ settings, datePicker, selectedDate, onChange } as inputConfig) =
    let
        (DatePicker model) =
            datePicker

        visibleMonth =
            model.focused
                |> Maybe.withDefault
                    (Maybe.withDefault model.today selectedDate)

        -- Internally, we use the config with the actual model and visibleMonth
        config =
            { settings = settings
            , model = model
            , text = inputConfig.text
            , selectedDate = selectedDate
            , visibleMonth = visibleMonth
            , onChange = onChange
            }

        calendar =
            if model.open then
                calendarView config

            else
                []
    in
    Input.text
        (calendar
            ++ attributes
            ++ [ Events.onFocus <| onChange <| PickerChanged OpenCalendar
               , Events.onLoseFocus <| onChange <| PickerChanged CloseCalendar
               ]
        )
        { onChange = onChange << TextChanged
        , text = config.text
        , placeholder = settings.placeholder
        , label = settings.label
        }


calendarView :
    Config msg
    -> List (Attribute msg)
calendarView config =
    [ Element.below <|
        Element.column
            [ preventDefaultOnMouseDown config
            , Background.color <| Element.rgb255 255 255 255
            , Border.width 1
            , padding 8
            , spacing 4
            ]
            [ calendarHeader config
            , calendarTable config
            ]
    ]


calendarTable : Config msg -> Element msg
calendarTable config =
    Element.table [ spacing 4, centerX, centerY ]
        { data = currentWeeks config
        , columns = calendarColumns config
        }


calendarColumns : Config msg -> List (Element.Column (Week Date) msg)
calendarColumns config =
    let
        weekDays =
            calendarWeekDays config.settings.firstDayOfWeek

        toColumn index weekday =
            { header = Element.el [ Font.bold ] (Element.text weekday)
            , width = Element.fill
            , view =
                \week ->
                    Week.getDay index week
                        |> dayView config
            }
    in
    Week.toList (Week.indexedMap toColumn weekDays)


calendarWeekDays : Weekday -> Week String
calendarWeekDays firstDayOfWeek =
    let
        startDay =
            Date.floor (weekdayToInterval firstDayOfWeek) (Date.fromCalendarDate 2020 Jan 1)

        days =
            Date.range Date.Day 1 startDay (Date.add Date.Days 7 startDay)
    in
    Week.fromListWithDefault "X" (List.map (Date.format "EEEEEE") days)


calendarHeader : Config msg -> Element msg
calendarHeader { visibleMonth, onChange } =
    Element.row [ Element.width Element.fill, padding 8, Font.bold ]
        [ Element.el
            [ alignLeft
            , Element.pointer
            , Events.onClick <|
                onChange <|
                    PickerChanged <|
                        ChangeFocus (Date.add Date.Months -1 visibleMonth)
            ]
          <|
            Element.text "<<"
        , Element.el [ centerX ] <|
            Element.text <|
                Date.format "MMMM YYYY" visibleMonth
        , Element.el
            [ alignRight
            , Element.pointer
            , Events.onClick <|
                onChange <|
                    PickerChanged <|
                        ChangeFocus (Date.add Date.Months 1 visibleMonth)
            ]
          <|
            Element.text ">>"
        ]


dayView : Config msg -> Date -> Element msg
dayView ({ model, settings } as config) day =
    let
        focusedBackgr =
            Background.color (Element.rgb255 0x6C 0x75 0x7D)

        focusedAttr =
            if Date.month config.visibleMonth /= Date.month day then
                Just (Font.color (Element.rgb255 0x80 0x80 0x80))

            else if config.visibleMonth == day then
                Just focusedBackgr

            else
                Nothing

        todayAttr =
            if model.today == day then
                Just (Border.width 1)

            else
                Nothing

        selectedAttr =
            config.selectedDate
                |> Maybe.andThen
                    (\selectedDate ->
                        if selectedDate == day then
                            Just (Background.color settings.focusColor)

                        else
                            Just (Element.mouseOver [ focusedBackgr ])
                    )

        noNothing a b =
            case a of
                Just x ->
                    x :: b

                Nothing ->
                    b

        calculatedAttr =
            List.foldl noNothing
                []
                [ focusedAttr, todayAttr, selectedAttr ]
    in
    Element.el
        ((Events.onClick <| config.onChange <| DateChanged day)
            :: Element.pointer
            :: calculatedAttr
        )
        (Element.el [ centerX, centerY ] <| Element.text <| Date.format "dd" day)


currentWeeks : Config msg -> List (Week Date)
currentWeeks ({ settings } as config) =
    let
        weekdayInterval =
            weekdayToInterval settings.firstDayOfWeek

        firstOfMonth =
            Date.fromCalendarDate (Date.year config.visibleMonth) (Date.month config.visibleMonth) 1

        start =
            firstOfMonth
                |> Date.floor weekdayInterval

        end =
            Date.add Date.Months 1 firstOfMonth
                |> Date.ceiling weekdayInterval

        weekDays startDay =
            Date.range Date.Day 1 startDay (Date.add Date.Days 7 startDay)

        toWeek list =
            Week.fromListWithDefault (Date.fromOrdinalDate 2020 1) list
    in
    Date.range Date.Day 7 start end
        |> List.map (weekDays >> toWeek)


weekdayToInterval : Weekday -> Date.Interval
weekdayToInterval weekday =
    case weekday of
        Mon ->
            Date.Monday

        Tue ->
            Date.Tuesday

        Wed ->
            Date.Wednesday

        Thu ->
            Date.Thursday

        Fri ->
            Date.Friday

        Sat ->
            Date.Saturday

        Sun ->
            Date.Sunday



-- HELPERS


{-| -}
preventDefaultOnMouseDown : Config msg -> Attribute msg
preventDefaultOnMouseDown config =
    Element.htmlAttribute <|
        Html.Events.preventDefaultOn "mousedown" <|
            Decode.succeed ( config.onChange <| PickerChanged NothingToDo, True )
