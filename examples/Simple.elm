module Simple exposing (main)

import Browser
import Date exposing (Date)
import DatePicker exposing (ChangeEvent(..))
import Element
import Element.Input as Input
import Html exposing (Html)
import Maybe.Extra


type alias Model =
    { date : Maybe Date
    , dateText : String
    , datePicker : DatePicker.DatePicker
    }


type Msg
    = ToDatePicker (DatePicker.Msg Msg)


settings : DatePicker.Settings Msg
settings =
    DatePicker.defaultSettings <| Input.labelAbove [] <| Element.text "Pick A Date"


init : ( Model, Cmd Msg )
init =
    let
        ( datePicker, datePickerFx ) =
            DatePicker.init
    in
    ( { date = Nothing
      , dateText = ""
      , datePicker = datePicker
      }
    , Cmd.map ToDatePicker datePickerFx
    )


view : Model -> Html Msg
view model =
    Element.layout [] <|
        Element.map ToDatePicker <|
            DatePicker.view [Element.width Element.shrink, Element.centerX, Element.centerY]
                { settings = settings
                , datePicker = model.datePicker
                , text = model.dateText
                , selectedDate = model.date
                }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToDatePicker subMsg ->
            let
                ( newDatePicker, dateEvent ) =
                    DatePicker.update subMsg model.datePicker

                -- Remember to set the new date picker, no matter what the dateEvent is!
                newModel =
                    { model | datePicker = newDatePicker }
            in
            case dateEvent of
                DateChanged newDate ->
                    ( { newModel
                        | date = Just newDate
                        , dateText = Date.toIsoString newDate
                      }
                    , Cmd.none
                    )

                TextChanged newText ->
                    ( { newModel
                        | date =
                            Date.fromIsoString newText
                                |> Result.toMaybe
                                |> Maybe.Extra.orElse newModel.date
                        , dateText = newText
                      }
                    , Cmd.none
                    )

                DateCleared ->
                    ( { newModel
                        | date = Nothing
                        , dateText = ""
                      }
                    , Cmd.none
                    )

                Message myMsg ->
                    update myMsg model

                None ->
                    ( newModel
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
