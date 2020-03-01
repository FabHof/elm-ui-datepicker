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
    , pickerModel : DatePicker.Model
    }


type Msg
    = ChangePicker ChangeEvent
    | SetToday Date


init : ( Model, Cmd Msg )
init =
    ( { date = Nothing
      , dateText = ""
      , pickerModel = DatePicker.init
      }
    , Task.perform SetToday Date.today
    )


view : Model -> Html Msg
view model =
    Element.layout [ Element.width Element.shrink ] <|
        DatePicker.input [ Element.centerX, Element.centerY ]
            { onChange = ChangePicker
            , selected = model.date
            , text = model.dateText
            , label =
                Input.labelAbove [] <|
                    Element.text "Pick A Date"
            , placeholder = Just <| Input.placeholder [] <| Element.text "jjjj-MM-dd"
            , settings = DatePicker.defaultSettings
            , model = model.pickerModel
            }


update : Msg -> Model -> ( Model, Cmd Msg )
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

                PickerChanged subMsg ->
                    -- internal stuff changed
                    -- call DatePicker.update
                    ( { model
                        | pickerModel =
                            model.pickerModel
                                |> DatePicker.update subMsg
                                |> DatePicker.open
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
