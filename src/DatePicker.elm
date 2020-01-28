module DatePicker exposing
    ( view, DatePicker, Msg, Settings, ChangeEvent(..), defaultSettings, init, update
    , labelAbove, labelBelow, labelLeft, labelRight, labelHidden, placeholder
    )

{-|


# Main

@docs view, DatePicker, Msg, Settings, ChangeEvent, defaultSettings, init, update


# Labels and Placeholders

@docs labelAbove, labelBelow, labelLeft, labelRight, labelHidden, placeholder

-}

import Date exposing (Date)
import Element exposing (Attribute, Element, alignLeft, alignRight, centerX, centerY, padding, spacing)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input exposing (Label, Placeholder)
import Html.Events
import Internal.Week as Week exposing (Week)
import Json.Decode as Decode
import Task
import Time exposing (Month(..), Weekday(..))


{-| Settings are the sutff you probably care about, but want to only set once for a given DatePicker.
-}
type alias Settings msg =
    { label : Input.Label (Msg msg)
    , placeholder : Maybe (Input.Placeholder (Msg msg))
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
init : ( DatePicker, Cmd (Msg msg) )
init =
    ( DatePicker
        { open = False
        , focused = Nothing
        , today = Date.fromOrdinalDate 1 1
        }
    , Date.today |> Task.perform ChangeToday
    )


{-| -}
type Msg msg
    = ChangeDate Date
    | ChangeText String
    | ChangeFocus Date
    | ChangeToday Date
    | OpenCalendar
    | CloseCalendar
    | NothingToDo
    | ExternalMsg msg


{-| -}
type ChangeEvent msg
    = DateChanged Date
    | DateCleared
    | TextChanged String
    | Message msg
    | None


{-| -}
update : Msg msg -> DatePicker -> ( DatePicker, ChangeEvent msg )
update msg (DatePicker model) =
    case msg of
        ChangeToday today ->
            ( DatePicker { model | today = today }, None )

        ChangeFocus focus ->
            ( DatePicker { model | focused = Just focus }, None )

        ChangeDate date ->
            ( DatePicker { model | focused = Just date }, DateChanged date )

        ChangeText text ->
            ( DatePicker model, TextChanged text )

        OpenCalendar ->
            ( DatePicker { model | open = True }, None )

        CloseCalendar ->
            ( DatePicker { model | open = False }, None )

        NothingToDo ->
            ( DatePicker model, None )

        ExternalMsg subMsg ->
            ( DatePicker model, Message subMsg )


{-| Reasonable default settings. You still have to give it some input.
-}
defaultSettings : Input.Label (Msg msg) -> Settings msg
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
        }
    -> Element (Msg msg)
view attributes { settings, datePicker, text, selectedDate } =
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
            , text = text
            , selectedDate = selectedDate
            , visibleMonth = visibleMonth
            }

        calendar =
            if model.open then
                calendarView config

            else
                []

        extAttributes =
            List.map (Element.mapAttribute ExternalMsg) attributes
    in
    Input.text
        (calendar
            ++ extAttributes
            ++ [ Events.onFocus OpenCalendar
               , Events.onLoseFocus CloseCalendar
               ]
        )
        { onChange = ChangeText
        , text = config.text
        , placeholder = settings.placeholder
        , label = settings.label
        }


calendarView :
    Config msg
    -> List (Attribute (Msg msg))
calendarView config =
    [ Element.below <|
        Element.column
            [ preventDefaultOnMouseDown
            , Background.color <| Element.rgb255 255 255 255
            , Border.width 1
            , padding 8
            , spacing 4
            ]
            [ calendarHeader config
            , calendarTable config
            ]
    ]


calendarTable : Config msg -> Element (Msg msg)
calendarTable config =
    Element.table [ spacing 4, centerX, centerY ]
        { data = currentWeeks config
        , columns = calendarColumns config
        }


calendarColumns : Config msg -> List (Element.Column (Week Date) (Msg msg))
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


calendarHeader : Config msg -> Element (Msg msg)
calendarHeader { visibleMonth } =
    Element.row [ Element.width Element.fill, padding 8, Font.bold ]
        [ Element.el
            [ alignLeft
            , Element.pointer
            , Events.onClick <| ChangeFocus (Date.add Date.Months -1 visibleMonth)
            ]
          <|
            Element.text "<<"
        , Element.el [ centerX ] <|
            Element.text <|
                Date.format "MMMM YYYY" visibleMonth
        , Element.el
            [ alignRight
            , Element.pointer
            , Events.onClick <| ChangeFocus (Date.add Date.Months 1 visibleMonth)
            ]
          <|
            Element.text ">>"
        ]


dayView : Config msg -> Date -> Element (Msg msg)
dayView ({ model, settings } as config) day =
    let
        focusedBackgr =
            Background.color (Element.rgb255 0x6C 0x75 0x7D)

        focusedAttr =
            model.focused
                |> Maybe.andThen
                    (\focusedDate ->
                        if Date.month focusedDate /= Date.month day then
                            Just (Font.color (Element.rgb255 0x80 0x80 0x80))

                        else if focusedDate == day then
                            Just focusedBackgr

                        else
                            Nothing
                    )

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
    Element.el (Events.onClick (ChangeDate day) :: Element.pointer :: calculatedAttr)
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


{-| -}
labelAbove : List (Attribute msg) -> Element msg -> Label (Msg msg)
labelAbove =
    fromInputLabel Input.labelAbove


{-| -}
labelBelow : List (Attribute msg) -> Element msg -> Label (Msg msg)
labelBelow =
    fromInputLabel Input.labelBelow


{-| -}
labelLeft : List (Attribute msg) -> Element msg -> Label (Msg msg)
labelLeft =
    fromInputLabel Input.labelLeft


{-| -}
labelRight : List (Attribute msg) -> Element msg -> Label (Msg msg)
labelRight =
    fromInputLabel Input.labelRight


{-| -}
labelHidden : String -> Label (Msg msg)
labelHidden =
    Input.labelHidden


{-| -}
placeholder : List (Attribute msg) -> Element msg -> Placeholder (Msg msg)
placeholder attr el =
    Input.placeholder (List.map (Element.mapAttribute ExternalMsg) attr) (Element.map ExternalMsg el)


fromInputLabel : (List (Attribute (Msg msg)) -> Element (Msg msg) -> Label (Msg msg)) -> List (Attribute msg) -> Element msg -> Label (Msg msg)
fromInputLabel fn attr el =
    fn (List.map (Element.mapAttribute ExternalMsg) attr) (Element.map ExternalMsg el)



-- HELPERS


{-| -}
preventDefaultOnMouseDown : Attribute (Msg msg)
preventDefaultOnMouseDown =
    Element.htmlAttribute <|
        Html.Events.preventDefaultOn "mousedown" <|
            Decode.succeed ( NothingToDo, True )
