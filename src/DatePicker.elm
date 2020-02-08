module DatePicker exposing
    ( input, Model, init, setToday, ChangeEvent(..), update, Settings, defaultSettings, initWithToday
    , close, open
    , Language
    )

{-|


# Basic Usage

@docs input, Model, init, setToday, ChangeEvent, update, Settings, defaultSettings, initWithToday


# Helpers

For when you want to be more in control

@docs close, open
@docs Language

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


{-| Has all the internal model of the date picker.
-}
type Model
    = Model Picker


type alias Picker =
    { open : Bool
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
        , today = Date.fromOrdinalDate 1 1
        , visibleMonth = Date.fromOrdinalDate 1 1
        }


{-| Gives you the initial model of the date picker and sets the given date as today.
-}
initWithToday : Date -> Model
initWithToday today =
    Model
        { open = False
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


type Msg
    = ChangeMonth Date
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
    | PickerChanged Msg


{-| -}
update : Msg -> Model -> Model
update msg (Model picker) =
    case msg of
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
type alias Settings msg =
    { firstDayOfWeek : Weekday
    , language : Maybe Language
    , disabled : Date -> Bool
    , pickerAttributes : List (Attribute msg)
    , headerAttributes : List (Attribute msg)
    , tableAttributes : List (Attribute msg)
    , dayAttributes : List (Attribute msg)
    , wrongMonthDayAttributes : List (Attribute msg)
    , todayDayAttributes : List (Attribute msg)
    , selectedDayAttributes : List (Attribute msg)
    , disabledDayAttributes : List (Attribute msg)
    , previousMonthElement : Element msg
    , nextMonthElement : Element msg
    }


{-| Reasonable default settings.
-}
defaultSettings : Settings msg
defaultSettings =
    { firstDayOfWeek = Mon
    , language = Nothing
    , disabled = always False
    , pickerAttributes =
        [ Border.width 1
        , Border.color (Element.rgb255 186 189 182)
        , Border.roundEach
            { topLeft = 0
            , topRight = 0
            , bottomLeft = 3
            , bottomRight = 3
            }
        , Element.moveDown 3
        , padding 8
        , spacing 4
        , Background.color <| Element.rgb255 255 255 255
        ]
    , headerAttributes =
        [ Element.width Element.fill
        , padding 8
        , Font.bold
        ]
    , tableAttributes = [ spacing 4, centerX, centerY ]
    , dayAttributes =
        [ Element.paddingXY 4 2
        , Border.rounded 3
        ]
    , wrongMonthDayAttributes =
        [ Font.color (Element.rgb255 0x80 0x80 0x80) ]
    , todayDayAttributes =
        [ Background.color (Element.rgb255 0xFF 0xC1 0x9B) ]
    , selectedDayAttributes =
        [ Background.color (Element.rgb255 0x00 0x7B 0xFF) ]
    , disabledDayAttributes =
        [ Font.color (Element.rgb255 0x80 0x80 0x80)
        , Background.color (Element.rgb255 0xDD 0xDD 0xDD)
        ]
    , previousMonthElement =
        Element.text "◄"
    , nextMonthElement =
        Element.text "►"
    }


{-| Alias of [`Language`][dateLanguage] from `justinmimbs/date`.
[dateLanguage]: <https://package.elm-lang.org/packages/justinmimbs/date/latest/Date#Language>
-}
type alias Language =
    Date.Language


type alias Config msg =
    { settings : Settings msg
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
        , settings : Settings msg
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

        pickerEl =
            if picker.open then
                pickerView config

            else
                []

        inputAttributes =
            if picker.open then
                attributes

            else
                (PickerChanged Open
                    |> onChange
                    |> Events.onClick
                )
                    :: attributes
    in
    Element.el inputAttributes
        (Input.text
            (pickerEl
                ++ [ Events.onFocus <| onChange <| PickerChanged Open
                   , Events.onLoseFocus <| onChange <| PickerChanged Close
                   ]
            )
            { onChange = onChange << TextChanged
            , text = config.text
            , placeholder = placeholder
            , label = label
            }
        )


pickerView :
    Config msg
    -> List (Attribute msg)
pickerView ({ settings } as config) =
    [ Element.below <|
        Element.column
            (preventDefaultOnMouseDown config
                :: settings.pickerAttributes
            )
            [ pickerHeader config
            , pickerTable config
            ]
    ]


pickerTable : Config msg -> Element msg
pickerTable ({ settings } as config) =
    Element.table settings.tableAttributes
        { data = currentWeeks config
        , columns = pickerColumns config
        }


pickerColumns : Config msg -> List (Element.Column (Week Date) msg)
pickerColumns config =
    let
        weekDays =
            calendarWeekDays config.settings

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
pickerHeader { visibleMonth, onChange, settings } =
    Element.row settings.headerAttributes
        [ Element.el
            [ alignLeft
            , Element.pointer
            , Events.onClick <|
                onChange <|
                    PickerChanged <|
                        ChangeMonth (Date.add Date.Months -1 visibleMonth)
            ]
          <|
            settings.previousMonthElement
        , Element.el [ centerX ] <|
            Element.text <|
                formatDate settings "MMMM YYYY" visibleMonth
        , Element.el
            [ alignRight
            , Element.pointer
            , Events.onClick <|
                onChange <|
                    PickerChanged <|
                        ChangeMonth (Date.add Date.Months 1 visibleMonth)
            ]
          <|
            settings.nextMonthElement
        ]


dayView : Config msg -> Date -> Element msg
dayView ({ picker, settings } as config) day =
    let
        attributesForThisDay =
            List.concat
                [ settings.dayAttributes
                , if Date.month config.visibleMonth /= Date.month day then
                    settings.wrongMonthDayAttributes

                  else
                    []
                , if picker.today == day then
                    settings.todayDayAttributes

                  else
                    []
                , if config.selected == Just day then
                    settings.selectedDayAttributes

                  else
                    []
                , if settings.disabled day then
                    settings.disabledDayAttributes

                  else
                    [ Events.onClick <| config.onChange <| DateChanged day, Element.pointer ]
                ]
    in
    Element.el attributesForThisDay
        (Element.text <| formatDate settings "dd" day)



-- STUFF WITH WEEKS AND DAYS


calendarWeekDays : Settings msg -> Week String
calendarWeekDays settings =
    let
        startDay =
            Date.floor (weekdayToInterval settings.firstDayOfWeek) (Date.fromCalendarDate 2020 Jan 1)

        days =
            Date.range Date.Day 1 startDay (Date.add Date.Days 7 startDay)
    in
    Week.fromListWithDefault "X" (List.map (formatDate settings "EEEEEE") days)


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


formatDate : Settings msg -> String -> Date -> String
formatDate settings string =
    case settings.language of
        Just language ->
            Date.formatWithLanguage language string

        Nothing ->
            Date.format string



-- ADDITIONAL HELPERS


{-| This is used, to prevent that the picker is closed unexpectedly.
-}
preventDefaultOnMouseDown : Config msg -> Attribute msg
preventDefaultOnMouseDown config =
    Element.htmlAttribute <|
        Html.Events.preventDefaultOn "mousedown" <|
            Decode.succeed ( config.onChange <| PickerChanged NothingToDo, True )
