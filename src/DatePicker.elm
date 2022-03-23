module DatePicker exposing
    ( input, Model, init, setToday, ChangeEvent(..), update, Settings, defaultSettings, initWithToday
    , close, open, setVisibleMonth
    , SelectorLevel(..), setSelectorLevel
    )

{-|


# Basic Usage

@docs input, Model, init, setToday, ChangeEvent, update, Settings, defaultSettings, initWithToday


# Helpers

For when you want to be more in control

@docs close, open, setVisibleMonth

-}

import Date exposing (Date)
import Element exposing (Attribute, Element, alignLeft, alignRight, centerX, centerY, padding, spacing)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html.Events
import Internal.Date as Date
import Internal.TestHelper as TestHelper
import Internal.Week as Week exposing (Week)
import Json.Decode as Decode
import Time exposing (Month(..), Weekday(..))



-- MODEL


{-| -}
type Model
    = Model Picker


type alias Picker =
    { open : Bool
    , today : Date
    , visibleMonth : Date
    , level : SelectorLevel
    }


{-| The initial model of the date picker.
Easy to us in your own init function:

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
        , level = DaysLevel
        }


{-| The initial model of the date picker and sets the given date as today.
-}
initWithToday : Date -> Model
initWithToday today =
    Model
        { open = False
        , today = today
        , visibleMonth = today
        , level = DaysLevel
        }


{-| Sets the day that should be marked as today.
-}
setToday : Date -> Model -> Model
setToday today (Model picker) =
    Model
        { picker
            | today = today
            , visibleMonth =
                if picker.visibleMonth == Date.fromOrdinalDate 1 1 then
                    today

                else
                    picker.visibleMonth
        }


{-| Closes the date picker.

Example: close date picker on date input:

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

**Note**: the date picker will reopen on _focus_ and _click_.
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


{-| Sets the current visible month of the date picker.
-}
setVisibleMonth : Date -> Model -> Model
setVisibleMonth date (Model picker) =
    Model { picker | visibleMonth = date }


{-| Sets the selector level that is visible when date picker is open.
-}
setSelectorLevel : SelectorLevel -> Model -> Model
setSelectorLevel level (Model picker) =
    Model { picker | level = level }



--  UPDATE


type Msg
    = ChangeMonth Date
    | ChangeMonthAndLevel Date SelectorLevel
    | ChangeYearAndLevel Int SelectorLevel
    | Open
    | Close
    | ChangeLevel SelectorLevel
    | NothingToDo


{-| The different selector levels the date picker can show.
-}
type SelectorLevel
    = DaysLevel
    | MonthsLevel
    | YearsLevel


{-| Use in your update function:

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
update msg model =
    case msg of
        ChangeMonth month ->
            setVisibleMonth month model

        ChangeMonthAndLevel month view ->
            model
                |> setVisibleMonth month
                |> setSelectorLevel view

        ChangeYearAndLevel year view ->
            model
                |> setVisibleMonth (Date.fromOrdinalDate year 1)
                |> setSelectorLevel view

        Open ->
            open model

        Close ->
            close model

        ChangeLevel view ->
            setSelectorLevel view model

        NothingToDo ->
            model



-- VIEW


{-| -}
type alias Settings =
    { firstDayOfWeek : Weekday
    , language : Maybe Language
    , disabled : Date -> Bool
    , pickerAttributes : List (Attribute Never)
    , headerAttributes : List (Attribute Never)
    , tableAttributes : List (Attribute Never)
    , weekdayAttributes : List (Attribute Never)
    , dayAttributes : List (Attribute Never)
    , monthYearAttribute : List (Attribute Never)
    , wrongMonthDayAttributes : List (Attribute Never)
    , todayDayAttributes : List (Attribute Never)
    , selectedDayAttributes : List (Attribute Never)
    , disabledDayAttributes : List (Attribute Never)
    , monthsTableAttributes : List (Attribute Never)
    , yearsTableAttributes : List (Attribute Never)
    , previousMonthElement : Element Never
    , nextMonthElement : Element Never
    }


{-| Reasonable default settings.
-}
defaultSettings : Settings
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
        , Element.centerX
        , Element.centerY
        , Element.width Element.fill
        , Background.color <| Element.rgb255 255 255 255
        ]
    , headerAttributes =
        [ Element.width Element.fill
        , padding 2
        , Font.bold
        ]
    , tableAttributes =
        [ Element.height Element.fill
        , Element.centerY
        ]
    , weekdayAttributes = [ Font.bold ]
    , dayAttributes =
        [ Border.rounded 3
        , padding 3
        , Element.width Element.fill
        , Font.center
        , Element.centerY
        , Element.mouseOver [ Background.color (Element.rgb255 0x73 0xB6 0xFF) ]
        ]
    , monthYearAttribute =
        [ Border.rounded 3
        , padding 11
        , Element.width Element.fill
        , Font.center
        , Element.centerY
        , Element.mouseOver [ Background.color (Element.rgb255 0x73 0xB6 0xFF) ]
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
    , monthsTableAttributes =
        [ Element.spaceEvenly
        , Element.width Element.fill
        , Element.height Element.fill
        ]
    , yearsTableAttributes =
        [ Element.spaceEvenly
        , Element.width Element.fill
        , Element.height Element.fill
        ]
    , previousMonthElement =
        Element.text "<"
    , nextMonthElement =
        Element.text ">"
    }


{-| Alias of [`Language`][dateLanguage] from `justinmimbs/date`.
[dateLanguage]: <https://package.elm-lang.org/packages/justinmimbs/date/latest/Date#Language>
-}
type alias Language =
    Date.Language


type alias Config msg =
    { settings : Settings
    , label : Input.Label msg
    , placeholder : Maybe (Input.Placeholder msg)
    , picker : Picker
    , text : String
    , selected : Maybe Date
    , onChange : ChangeEvent -> msg
    }


{-| Use it like you would `Input.text`, the attributes, `text`, `placeholder` and `label` will behave
exactly like for `Input.text`. It has however a more complex `onChange`, a `selected` date, the internal `model` and some `settings`.

**Note**: `Events.onClick`, `Events.onFocus` and `Events.onLoseFocus` are used internally by the date picker.
This means, that **your own `Events.onClick`, `Events.onFocus` and `Events.onLoseFocus` attributes have no effect and will not fire**.

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
                attributes
                    ++ [ Events.onClick <| onChange <| PickerChanged Open ]
    in
    Input.text
        (inputAttributes
            ++ pickerEl
            ++ [ Events.onFocus <| onChange <| PickerChanged Open
               , Events.onLoseFocus <| onChange <| PickerChanged Close
               , TestHelper.inputAttr
               ]
        )
        { onChange = onChange << TextChanged
        , text = config.text
        , placeholder = placeholder
        , label = label
        }


pickerView :
    Config msg
    -> List (Attribute msg)
pickerView ({ settings } as config) =
    [ Element.below <|
        Element.column
            (TestHelper.calendarAttr
                :: preventDefaultOnMouseDown config
                :: extAttrs settings.pickerAttributes
            )
            [ pickerHeader config
            , pickerTable config
            ]
    ]


pickerTable : Config msg -> Element msg
pickerTable ({ settings, picker } as config) =
    case picker.level of
        DaysLevel ->
            Element.table (TestHelper.tableAttr :: extAttrs settings.tableAttributes)
                { data = Week.weeksInMonth picker.visibleMonth config.settings.firstDayOfWeek
                , columns = pickerColumns config
                }

        MonthsLevel ->
            monthTable config

        YearsLevel ->
            let
                decade =
                    Date.year picker.visibleMonth // 10 * 10
            in
            Element.column (TestHelper.tableAttr :: extAttrs settings.yearsTableAttributes)
                (List.range 0 2
                    |> List.map
                        (\i ->
                            Element.row (extAttrs settings.yearsTableAttributes)
                                (List.range -1 2
                                    |> List.map
                                        (\j ->
                                            4
                                                * i
                                                + j
                                                + decade
                                                |> selectYearElement config
                                        )
                                )
                        )
                )


monthTable : Config msg -> Element msg
monthTable ({ settings, picker } as config) =
    Element.column (TestHelper.tableAttr :: extAttrs settings.monthsTableAttributes)
        (List.range 0 2
            |> List.map
                (\i ->
                    Element.row (extAttrs settings.monthsTableAttributes)
                        (List.range 0 3
                            |> List.map
                                (\j ->
                                    Date.fromCalendarDate (Date.year picker.visibleMonth) Jan 1
                                        |> Date.add Date.Months (4 * i + j)
                                        |> selectMonthElement config
                                )
                        )
                )
        )


selectYearElement : Config msg -> Int -> Element msg
selectYearElement ({ settings, picker } as config) year =
    let
        attributesForThisYear =
            List.concat
                [ extAttrs settings.monthYearAttribute
                , [ Events.onClick <| config.onChange <| PickerChanged <| ChangeYearAndLevel year MonthsLevel
                  , Element.pointer
                  , TestHelper.yearAttr
                  ]
                , if
                    Date.year picker.today
                        == year
                  then
                    TestHelper.todayAttr
                        :: extAttrs settings.todayDayAttributes

                  else
                    []
                , if
                    Maybe.map Date.year config.selected
                        == Just year
                  then
                    TestHelper.selectedAttr
                        :: extAttrs settings.selectedDayAttributes

                  else
                    []
                ]
    in
    Element.el attributesForThisYear
        (Element.text <| String.fromInt year)


selectMonthElement : Config msg -> Date -> Element msg
selectMonthElement ({ settings, picker } as config) month =
    let
        attributesForThisMonth =
            List.concat
                [ extAttrs settings.monthYearAttribute
                , [ Events.onClick <| config.onChange <| PickerChanged <| ChangeMonthAndLevel month DaysLevel
                  , Element.pointer
                  , TestHelper.monthAttr
                  ]
                , if
                    Date.month picker.today
                        == Date.month month
                        && Date.year picker.today
                        == Date.year month
                  then
                    TestHelper.todayAttr
                        :: extAttrs settings.todayDayAttributes

                  else
                    []
                , if
                    Maybe.map Date.month config.selected
                        == Just (Date.month month)
                        && Maybe.map Date.year config.selected
                        == Just (Date.year month)
                  then
                    TestHelper.selectedAttr
                        :: extAttrs settings.selectedDayAttributes

                  else
                    []

                -- TODO All Days in Month are disabled => Disable Month?
                -- , if settings.disabled day then
                --     extAttrs settings.disabledDayAttributes
                --   else
                --     [ Events.onClick <| config.onChange <| DateChanged day, Element.pointer ]
                ]
    in
    Element.el attributesForThisMonth
        (Element.text <| Date.formatMaybeLanguage settings.language "MMM" month)


pickerColumns : Config msg -> List (Element.Column (Week Date) msg)
pickerColumns config =
    let
        weekdays =
            Week.calendarWeekDays config.settings.firstDayOfWeek config.settings.language

        toColumn index weekday =
            { header = Element.el (extAttrs config.settings.weekdayAttributes) (Element.text weekday)
            , width = Element.fill
            , view =
                \week ->
                    Week.getDay index week
                        |> dayView config
            }
    in
    Week.toList (Week.indexedMap toColumn weekdays)


pickerHeader : Config msg -> Element msg
pickerHeader { onChange, picker, settings } =
    case picker.level of
        DaysLevel ->
            Element.row (extAttrs settings.headerAttributes)
                [ Element.el
                    [ alignLeft
                    , Element.pointer
                    , Events.onClick <|
                        onChange <|
                            PickerChanged <|
                                ChangeMonth (Date.add Date.Months -1 picker.visibleMonth)
                    , TestHelper.previousMonthAttr
                    ]
                  <|
                    extEle settings.previousMonthElement
                , Element.el
                    [ centerX
                    , Element.pointer
                    , Events.onClick <| onChange <| PickerChanged <| ChangeLevel MonthsLevel
                    ]
                  <|
                    Element.text <|
                        Date.formatMaybeLanguage settings.language "MMMM yyyy" picker.visibleMonth
                , Element.el
                    [ alignRight
                    , Element.pointer
                    , Events.onClick <|
                        onChange <|
                            PickerChanged <|
                                ChangeMonth (Date.add Date.Months 1 picker.visibleMonth)
                    , TestHelper.nextMonthAttr
                    ]
                  <|
                    extEle settings.nextMonthElement
                ]

        MonthsLevel ->
            Element.row (extAttrs settings.headerAttributes)
                [ Element.el
                    [ alignLeft
                    , Element.pointer
                    , Events.onClick <|
                        onChange <|
                            PickerChanged <|
                                ChangeMonth (Date.add Date.Years -1 picker.visibleMonth)
                    , TestHelper.previousMonthAttr
                    ]
                  <|
                    extEle settings.previousMonthElement
                , Element.el
                    [ centerX
                    , Element.pointer
                    , Events.onClick <| onChange <| PickerChanged <| ChangeLevel YearsLevel
                    ]
                  <|
                    Element.text <|
                        Date.formatMaybeLanguage settings.language "yyyy" picker.visibleMonth
                , Element.el
                    [ alignRight
                    , Element.pointer
                    , Events.onClick <|
                        onChange <|
                            PickerChanged <|
                                ChangeMonth (Date.add Date.Years 1 picker.visibleMonth)
                    , TestHelper.nextMonthAttr
                    ]
                  <|
                    extEle settings.nextMonthElement
                ]

        YearsLevel ->
            Element.row (extAttrs settings.headerAttributes)
                [ Element.el
                    [ alignLeft
                    , Element.pointer
                    , Events.onClick <|
                        onChange <|
                            PickerChanged <|
                                ChangeMonth (Date.add Date.Years -10 picker.visibleMonth)
                    , TestHelper.previousMonthAttr
                    ]
                  <|
                    extEle settings.previousMonthElement
                , Element.el
                    [ centerX
                    ]
                  <|
                    Element.text <|
                        (Date.formatMaybeLanguage settings.language "yyyy" picker.visibleMonth
                            |> String.slice 0 3
                            |> String.padRight 4 'X'
                        )
                , Element.el
                    [ alignRight
                    , Element.pointer
                    , Events.onClick <|
                        onChange <|
                            PickerChanged <|
                                ChangeMonth (Date.add Date.Years 10 picker.visibleMonth)
                    , TestHelper.nextMonthAttr
                    ]
                  <|
                    extEle settings.nextMonthElement
                ]


dayView : Config msg -> Date -> Element msg
dayView ({ picker, settings } as config) day =
    let
        attributesForThisDay =
            List.concat
                [ extAttrs settings.dayAttributes
                , if Date.month picker.visibleMonth /= Date.month day then
                    extAttrs settings.wrongMonthDayAttributes

                  else
                    [ TestHelper.dayInMonthAttr ]
                , if picker.today == day then
                    TestHelper.todayAttr
                        :: extAttrs settings.todayDayAttributes

                  else
                    []
                , if config.selected == Just day then
                    TestHelper.selectedAttr
                        :: extAttrs settings.selectedDayAttributes

                  else
                    []
                , if settings.disabled day then
                    extAttrs settings.disabledDayAttributes

                  else
                    [ Events.onClick <| config.onChange <| DateChanged day, Element.pointer ]
                ]
    in
    Element.el attributesForThisDay
        (Element.text <| Date.formatMaybeLanguage settings.language "dd" day)



-- ADDITIONAL HELPERS


extAttrs : List (Attribute Never) -> List (Attribute a)
extAttrs =
    List.map (Element.mapAttribute never)


extEle : Element Never -> Element a
extEle =
    Element.map never


{-| This is used, to prevent that the picker is closed unexpectedly.
-}
preventDefaultOnMouseDown : Config msg -> Attribute msg
preventDefaultOnMouseDown config =
    Element.htmlAttribute <|
        Html.Events.preventDefaultOn "mousedown" <|
            Decode.succeed ( config.onChange <| PickerChanged NothingToDo, True )
