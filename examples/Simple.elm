module Simple exposing (main)

import Browser
import Date exposing (Date)
import DatePicker exposing (ChangeEvent(..))
import Element
import Element.Input as Input
import Html exposing (Html)
import Maybe.Extra
import Task


type alias Model =
    { date : Maybe Date
    , dateText : String
    , datePicker : DatePicker.DatePicker
    }


type Msg
    = DatePickerChanged ChangeEvent
    | ChangeTody Date


init : ( Model, Cmd Msg )
init =
    ( { date = Nothing
      , dateText = ""
      , datePicker = DatePicker.init
      }
    , Date.today |> Task.perform ChangeTody
    )


view : Model -> Html Msg
view model =
    Element.layout [] <|
        DatePicker.view [ Element.width Element.shrink, Element.centerX, Element.centerY ]
            { settings = DatePicker.defaultSettings
            , label = Input.labelAbove [] <| Element.text "Pick A Date"
            , placeholder = Just <| Input.placeholder [] <| Element.text "jjjj-MM-dd"
            , datePicker = model.datePicker
            , text = model.dateText
            , selectedDate = model.date
            , onChange = DatePickerChanged
            }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DatePickerChanged changeEvent ->
            case changeEvent of
                DateChanged date ->
                    -- remember to update both the date and the text
                    ( { model
                        | date = Just date
                        , dateText = Date.toIsoString date
                        , datePicker = DatePicker.close model.datePicker
                      }
                    , Cmd.none
                    )

                TextChanged text ->
                    ( { model
                        | date =
                            -- parse the input text in any way you like
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
                    ( { model | datePicker = DatePicker.update subMsg model.datePicker }
                    , Cmd.none
                    )

        ChangeTody today ->
            ( { model | datePicker = DatePicker.setToday today model.datePicker }
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
