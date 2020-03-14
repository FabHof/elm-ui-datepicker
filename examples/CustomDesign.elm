module CustomDesign exposing (main)

import Browser
import Date exposing (Date)
import DatePicker exposing (ChangeEvent(..))
import Element exposing (padding)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
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
      , pickerModel =
            DatePicker.init
                |> DatePicker.open
      }
    , Task.perform SetToday Date.today
    )


view : Model -> Html Msg
view model =
    Element.layout [] <|
        DatePicker.input [ Element.width Element.shrink, Element.centerX, Element.centerY ]
            { onChange = ChangePicker
            , selected = model.date
            , text = model.dateText
            , label = Input.labelAbove [] <| Element.text "Pick A Date"
            , placeholder = Just <| Input.placeholder [] <| Element.text "jjjj-MM-dd"
            , settings = settings
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


{-| If you want to have the date picker look crazy - you can do it!
-}
settings : DatePicker.Settings
settings =
    let
        default =
            DatePicker.defaultSettings
    in
    { default
        | pickerAttributes =
            [ Border.width 1
            , Border.color (Element.rgb255 186 189 182)
            ]
        , headerAttributes =
            [ Element.width Element.fill
            , Font.bold
            , Background.color (Element.rgb255 187 60 99)
            ]
        , tableAttributes =
            [ padding 6, Background.color (Element.rgb255 190 210 150) ]
        , dayAttributes = []
        , wrongMonthDayAttributes =
            [ Font.light ]
        , selectedDayAttributes =
            [ Background.color (Element.rgb255 95 15 255) ]
        , previousMonthElement =
            Element.el
                [ Background.color (Element.rgb255 60 187 99)
                , padding 12
                ]
            <|
                Element.text
                    "<"
        , nextMonthElement =
            Element.el
                [ Background.color (Element.rgb255 60 187 99)
                , padding 12
                ]
            <|
                Element.text
                    ">"
    }


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
