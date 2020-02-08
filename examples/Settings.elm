module Settings exposing (main)

import Browser
import Date exposing (Date)
import DatePicker exposing (ChangeEvent(..))
import Element
import Element.Input as Input
import Html exposing (Html)
import Maybe.Extra
import Parser exposing ((|.), (|=), Parser, chompWhile, getChompedString, problem, succeed, symbol)
import Task
import Time exposing (Month(..), Weekday(..))


type alias Model =
    { date : Maybe Date
    , dateText : String
    , pickerModel : DatePicker.Model
    }


type Msg
    = ChangePicker ChangeEvent
    | SetToday Date


init : ( Model, Cmd Msg )
init =
    ( { date = Nothing
      , dateText = ""
      , pickerModel = DatePicker.open DatePicker.init
      }
    , Task.perform SetToday Date.today
    )


settings : DatePicker.Settings msg
settings =
    let
        default =
            DatePicker.defaultSettings
    in
    { default
        | language = Just language
        , disabled =
            \day ->
                Date.weekday day == Sun
    }


language : DatePicker.Language
language =
    { monthName = monthName True
    , monthNameShort = monthName False
    , weekdayName = weekdayName True
    , weekdayNameShort = weekdayName False
    , dayWithSuffix = \x -> String.fromInt x ++ "."
    }


weekdayName : Bool -> Weekday -> String
weekdayName long weekday =
    let
        longName =
            case weekday of
                Mon ->
                    "Montag"

                Tue ->
                    "Dienstag"

                Wed ->
                    "Mittwoch"

                Thu ->
                    "Donnerstag"

                Fri ->
                    "Freitag"

                Sat ->
                    "Samstag"

                Sun ->
                    "Sonntag"
    in
    if long then
        longName

    else
        String.left 3 longName


monthName : Bool -> Month -> String
monthName long month =
    let
        longName =
            case month of
                Jan ->
                    "Januar"

                Feb ->
                    "Februar"

                Mar ->
                    "März"

                Apr ->
                    "April"

                May ->
                    "Mai"

                Jun ->
                    "Juni"

                Jul ->
                    "Juli"

                Aug ->
                    "August"

                Sep ->
                    "September"

                Oct ->
                    "Oktober"

                Nov ->
                    "November"

                Dec ->
                    "Dezember"
    in
    if long then
        longName

    else
        String.left 3 longName


view : Model -> Html Msg
view model =
    Element.layout [] <|
        DatePicker.input [ Element.width Element.shrink, Element.centerX, Element.centerY ]
            { onChange = ChangePicker
            , selected = model.date
            , text = model.dateText
            , label = Input.labelAbove [] <| Element.text "Pick A Date"
            , placeholder = Nothing
            , settings = settings
            , model = model.pickerModel
            }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangePicker changeEvent ->
            case changeEvent of
                DateChanged date ->
                    ( { model
                        | date = Just date
                        , dateText = Date.formatWithLanguage language "EEEE, ddd MMMM y" date
                      }
                    , Cmd.none
                    )

                TextChanged text ->
                    ( { model
                        | date =
                            Parser.run germanDateParser text
                                -- Date.fromIsoString text
                                |> Result.toMaybe
                                |> Maybe.Extra.orElse model.date
                        , dateText = text
                      }
                    , Cmd.none
                    )

                PickerChanged subMsg ->
                    ( { model
                        | pickerModel =
                            model.pickerModel
                                |> DatePicker.update subMsg
                      }
                    , Cmd.none
                    )

        SetToday today ->
            ( { model
                | pickerModel =
                    model.pickerModel
                        |> DatePicker.setToday today
              }
            , Cmd.none
            )


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias MyDate =
    { day : Int
    , month : Month
    , year : Int
    }


germanDateParser : Parser Date
germanDateParser =
    (succeed MyDate
        |= (digits
                |> min 1
                |> max 31
           )
        |. symbol "."
        |= (digits
                |> min 1
                |> max 12
                |> Parser.map Date.numberToMonth
           )
        |. symbol "."
        |= digits
    )
        |> Parser.map (\myDate -> Date.fromCalendarDate myDate.year myDate.month myDate.day)


digits : Parser Int
digits =
    getChompedString (chompWhile Char.isDigit)
        |> Parser.andThen
            (\str ->
                case String.toInt str of
                    Just n ->
                        succeed n

                    Nothing ->
                        problem "segment is not a number"
            )


min : Int -> Parser Int -> Parser Int
min minVal =
    Parser.andThen
        (\val ->
            if val < minVal then
                problem "value is to small"

            else
                succeed val
        )


max : Int -> Parser Int -> Parser Int
max maxVal =
    Parser.andThen
        (\val ->
            if val > maxVal then
                problem "value is to big"

            else
                succeed val
        )
