module DatePicker exposing
    ( input, Model, init, setToday, ChangeEvent(..), update, Settings, defaultSettings, initWithToday
    , close, open
    )

{-|


# Basic Usage

@docs input, Model, init, setToday, ChangeEvent, update, Settings, defaultSettings, initWithToday


# Helpers

For when you want to be more in control

@docs close, open

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



-- MODEL


{-| Has all the internal picker of the date picker.
-}
type Model
    = Model Picker


type alias Picker =
    { open : Bool
    , focused : Maybe Date
    , today : Date
    , visibleMonth : Date
    }


{-| Gives you the initial model of the date picker.
Easy to us in you own init function:

(You probably want to get todays date to give it to the date picker using [DatePicker.setToday](DatePicker#setToday))

    init =
        ( { date = Nothing
          , dateText = ""
          , pickerModel = DatePicker.init
          }
        , Task.perform SetToday Date.today
        )

-}
init : Model
init =
    Model
        { open = False
        , focused = Nothing
        , today = Date.fromOrdinalDate 1 1
        , visibleMonth = Date.fromOrdinalDate 1 1
        }


{-| Gives you the initial model of the date picker and sets the given date as today.
-}
initWithToday : Date -> Model
initWithToday today =
    Model
        { open = False
        , focused = Nothing
        , today = today
        , visibleMonth = today
        }


{-| Gives the date picker information about what date it is today.
Use this in your update function:

    update msg model =
        case msg of
            SetToday today ->
                ( { model
                    | pickerModel =
                        model.pickerModel
                            |> DatePicker.setToday today
                  }
                , Cmd.none
                )

-}
setToday : Date -> Model -> Model
setToday today (Model picker) =
    Model { picker | today = today, visibleMonth = today }


{-| Closes the date picker.

Common use case: close date picker on date input:

    DateChanged date ->
        ( { model
            | date = Just date
            , dateText = Date.toIsoString date
            , pickerModel =
                model.pickerModel
                    |> DatePicker.close
        }
        , Cmd.none
        )

**Note**: the date picker will reopen on _enter_ and _click_.
To prevent this, close the date picker on every update:

    PickerChanged subMsg ->
        ( { model
            | pickerModel =
                model.pickerModel
                    |> DatePicker.update subMsg
                    --picker will never open
                    |> DatePicker.close
            }
        , Cmd.none
        )

-}
close : Model -> Model
close (Model picker) =
    Model
        { picker
            | open = False
            , focused = Nothing
        }


{-| Opens the date picker

Example: start with open picker:

    init : ( Model, Cmd Msg )
    init =
        ( { date = Nothing
          , dateText = ""
          , pickerModel =
                DatePicker.init
                    |> DatePicker.open
          }
        , Task.perform SetToday Date.today
        )

-}
open : Model -> Model
open (Model picker) =
    Model { picker | open = True }



--  UPDATE


{-| -}
type Msg
    = ChangeFocus (Maybe Date)
    | ChangeMonth Date
    | Open
    | Close
    | NothingToDo


{-| Simple example of us in a update function:

    update msg model =
        case msg of
            ChangePicker changeEvent ->
                case changeEvent of
                    DateChanged date ->
                        -- update both date and text
                        ( { model
                            | date = Just date
                            , dateText = Date.toIsoString date
                          }
                        , Cmd.none
                        )

                    TextChanged text ->
                        ( { model
                            | date =
                                -- parse the text in any way you like
                                Date.fromIsoString text
                                    |> Result.toMaybe
                                    |> Maybe.Extra.orElse model.date
                            , dateText = text
                          }
                        , Cmd.none
                        )

                    DateCleared ->
                        ( { model
                            | date =
                                Nothing
                            , dateText = ""
                          }
                        , Cmd.none
                        )

                    PickerChanged subMsg ->
                        -- internal stuff changed
                        -- call DatePicker.update
                        ( { model
                            | pickerModel =
                                model.pickerModel
                                    |> DatePicker.update subMsg
                          }
                        , Cmd.none
                        )

-}
type ChangeEvent
    = DateChanged Date
    | TextChanged String
    | DateCleared
    | PickerChanged Msg


{-| -}
update : Msg -> Model -> Model
update msg (Model picker) =
    case msg of
        ChangeFocus focus ->
            Model
                { picker
                    | focused = focus
                    , visibleMonth =
                        case focus of
                            Just date ->
                                date

                            Nothing ->
                                picker.visibleMonth
                }

        ChangeMonth month ->
            Model { picker | visibleMonth = month }

        Open ->
            open (Model picker)

        Close ->
            close (Model picker)

        NothingToDo ->
            Model picker



-- VIEW


{-| -}
type alias Settings =
    { firstDayOfWeek : Weekday
    , focusColor : Element.Color
    }


{-| Reasonable default settings.
-}
defaultSettings : Settings
defaultSettings =
    { firstDayOfWeek = Mon
    , focusColor = Element.rgb255 0x00 0x7B 0xFF
    }


type alias Config msg =
    { settings : Settings
    , label : Input.Label msg
    , placeholder : Maybe (Input.Placeholder msg)
    , picker : Picker
    , text : String
    , selected : Maybe Date
    , visibleMonth : Date
    , onChange : ChangeEvent -> msg
    }


{-| This view function is a wrapper around `Input.text`, with a more complex `onChange`, a `selected` date, the internal `model` and some `settings`
-}
input :
    List (Attribute msg)
    ->
        { onChange : ChangeEvent -> msg
        , selected : Maybe Date
        , text : String
        , label : Input.Label msg
        , placeholder : Maybe (Input.Placeholder msg)
        , model : Model
        , settings : Settings
        }
    -> Element msg
input attributes ({ settings, model, label, placeholder, selected, onChange } as inputConfig) =
    let
        (Model picker) =
            model

        config =
            { settings = settings
            , picker = picker
            , text = inputConfig.text
            , label = label
            , placeholder = placeholder
            , selected = selected
            , visibleMonth = picker.visibleMonth
            , onChange = onChange
            }

        calendar =
            if picker.open then
                pickerView config

            else
                []

        pickerAttributes =
            if picker.open then
                attributes

            else
                (PickerChanged Open
                    |> onChange
                    |> Events.onClick
                )
                    :: attributes
    in
    Element.el pickerAttributes
        (Input.text
            (calendar
                ++ [ Events.onFocus <| onChange <| PickerChanged Open
                   , Events.onLoseFocus <| onChange <| PickerChanged Close
                   , arrowKeyDownFocusChange config
                   ]
            )
            { onChange = onChange << TextChanged
            , text = config.text
            , placeholder = placeholder
            , label = label
            }
        )


arrowKeyDownFocusChange : Config msg -> Attribute msg
arrowKeyDownFocusChange ({ picker } as config) =
    let
        focus =
            case picker.focused of
                Just date ->
                    date

                Nothing ->
                    Maybe.withDefault picker.today config.selected

        toFocusMsg date =
            config.onChange <| PickerChanged <| ChangeFocus <| Just date

        toKeyCodeToMsg keyCode =
            case keyCode of
                40 ->
                    -- ArrowDown
                    ( Date.add Date.Days 7 focus
                        |> toFocusMsg
                    , False
                    )

                38 ->
                    -- ArrowUp
                    ( Date.add Date.Days -7 focus
                        |> toFocusMsg
                    , False
                    )

                -- On Left and Right, only move focus if we have something focused or if the text is empty
                -- Prevent Default (move cursor) when we move focus.
                -- TODO decide on *best* arrow key behaviour
                37 ->
                    -- ArrowLeft
                    case picker.focused of
                        Just date ->
                            ( Date.add Date.Days -1 date
                                |> toFocusMsg
                            , True
                            )

                        Nothing ->
                            if config.text == "" then
                                ( Date.add Date.Days -1 focus
                                    |> toFocusMsg
                                , True
                                )

                            else
                                ( NothingToDo
                                    |> PickerChanged
                                    |> config.onChange
                                , False
                                )

                39 ->
                    -- ArrowRight
                    case picker.focused of
                        Just date ->
                            ( Date.add Date.Days 1 date
                                |> toFocusMsg
                            , True
                            )

                        Nothing ->
                            if config.text == "" then
                                ( Date.add Date.Days 1 focus
                                    |> toFocusMsg
                                , True
                                )

                            else
                                ( NothingToDo
                                    |> PickerChanged
                                    |> config.onChange
                                , False
                                )

                13 ->
                    -- Enter
                    case picker.focused of
                        Just date ->
                            ( config.onChange <| DateChanged date, False )

                        Nothing ->
                            ( NothingToDo
                                |> PickerChanged
                                |> config.onChange
                            , False
                            )

                _ ->
                    ( NothingToDo
                        |> PickerChanged
                        |> config.onChange
                    , False
                    )
    in
    if picker.open then
        Element.htmlAttribute <|
            Html.Events.preventDefaultOn "keydown"
                (Html.Events.keyCode
                    |> Decode.map toKeyCodeToMsg
                )

    else
        Element.htmlAttribute <|
            Html.Events.on "keydown"
                (Html.Events.keyCode
                    |> Decode.andThen
                        (\keycode ->
                            if keycode == 13 then
                                Decode.succeed <| config.onChange <| PickerChanged Open

                            else
                                Decode.fail "Not enter"
                        )
                )


pickerView :
    Config msg
    -> List (Attribute msg)
pickerView config =
    [ Element.below <|
        Element.column
            [ preventDefaultOnMouseDown config
            , Background.color <| Element.rgb255 255 255 255
            , Border.width 1
            , padding 8
            , spacing 4
            ]
            [ pickerHeader config
            , pickerTable config
            ]
    ]


pickerTable : Config msg -> Element msg
pickerTable config =
    Element.table [ spacing 4, centerX, centerY ]
        { data = currentWeeks config
        , columns = pickerColumns config
        }


pickerColumns : Config msg -> List (Element.Column (Week Date) msg)
pickerColumns config =
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


pickerHeader : Config msg -> Element msg
pickerHeader { visibleMonth, onChange } =
    Element.row [ Element.width Element.fill, padding 8, Font.bold ]
        [ Element.el
            [ alignLeft
            , Element.pointer
            , Events.onClick <|
                onChange <|
                    PickerChanged <|
                        ChangeMonth (Date.add Date.Months -1 visibleMonth)
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
                        ChangeMonth (Date.add Date.Months 1 visibleMonth)
            ]
          <|
            Element.text ">>"
        ]


dayView : Config msg -> Date -> Element msg
dayView ({ picker, settings } as config) day =
    let
        focusedBackground =
            Background.color (Element.rgb255 0x6C 0x75 0x7D)

        wrongMonthAttr =
            if Date.month config.visibleMonth /= Date.month day then
                Just (Font.color (Element.rgb255 0x80 0x80 0x80))

            else
                Nothing

        focusedAttr =
            if picker.focused == Just day then
                Just focusedBackground

            else
                Nothing

        todayAttr =
            if picker.today == day then
                Just (Border.width 1)

            else
                Nothing

        selectedAttr =
            if config.selected == Just day then
                Just (Background.color settings.focusColor)

            else
                Nothing

        noNothing a b =
            case a of
                Just x ->
                    x :: b

                Nothing ->
                    b

        calculatedAttr =
            List.foldl noNothing
                []
                [ focusedAttr, todayAttr, selectedAttr, wrongMonthAttr ]
    in
    Element.el
        ((Events.onClick <| config.onChange <| DateChanged day)
            :: Element.pointer
            :: calculatedAttr
        )
        (Element.el [ centerX, centerY ] <| Element.text <| Date.format "dd" day)



-- STUFF WITH WEEKS AND DAYS


calendarWeekDays : Weekday -> Week String
calendarWeekDays firstDayOfWeek =
    let
        startDay =
            Date.floor (weekdayToInterval firstDayOfWeek) (Date.fromCalendarDate 2020 Jan 1)

        days =
            Date.range Date.Day 1 startDay (Date.add Date.Days 7 startDay)
    in
    Week.fromListWithDefault "X" (List.map (Date.format "EEEEEE") days)


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



-- ADDITIONAL HELPERS


{-| This is used, to prevent that the picker is closed unexpectedly.
-}
preventDefaultOnMouseDown : Config msg -> Attribute msg
preventDefaultOnMouseDown config =
    Element.htmlAttribute <|
        Html.Events.preventDefaultOn "mousedown" <|
            Decode.succeed ( config.onChange <| PickerChanged NothingToDo, True )
