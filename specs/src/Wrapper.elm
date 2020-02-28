module Wrapper exposing (..)

import DatePicker exposing (ChangeEvent(..))
import Element
import Element.Input as Input
import Html exposing (Html)
import Spec.Setup as Setup exposing (Setup)


simpleApp : Setup Model Msg
simpleApp =
    Setup.init init
        |> Setup.withUpdate update
        |> Setup.withView view


type alias Model =
    DatePicker.Model


type Msg
    = ChangePicker ChangeEvent
    | Open
    | Close


init : ( Model, Cmd Msg )
init =
    ( DatePicker.init
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    Element.layout [ Element.width Element.shrink ] <|
        DatePicker.input [ Element.centerX, Element.centerY ]
            { onChange = ChangePicker
            , selected = Nothing
            , text = ""
            , label =
                Input.labelAbove [] <|
                    Element.text "Pick A Date"
            , placeholder = Just <| Input.placeholder [] <| Element.text "jjjj-MM-dd"
            , settings = DatePicker.defaultSettings
            , model = model
            }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangePicker changeEvent ->
            case changeEvent of
                DateChanged date ->
                    ( model, Cmd.none )

                TextChanged text ->
                    ( model, Cmd.none )

                PickerChanged subMsg ->
                    -- internal stuff changed
                    -- call DatePicker.update
                    ( model
                        |> DatePicker.update subMsg
                    , Cmd.none
                    )

        Open ->
            ( model |> DatePicker.open, Cmd.none )

        Close ->
            ( model |> DatePicker.close, Cmd.none )
