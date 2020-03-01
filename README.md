# Date Picker

A reasonable date picker for the awesome [elm-ui](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/).

At it's core, this date picker is just a wrapper around [Element.Input.text](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/Element-Input#text).

[See it in action here.](https://fabhof.github.io/elm-ui-datepicker/)

## Usage

It makes the most sense if you look at the [simple example](https://github.com/FabHof/elm-ui-datepicker/blob/master/examples/Simple.elm) and the [other examples](https://github.com/FabHof/elm-ui-datepicker/tree/master/examples).

The date picker *has* an internal Model, but it does hold neither the selected date, nor the text of the underlying `Element.Input.text`. Therefore your minimal working model looks like this:

```elm
type alias Model =
    { date : Maybe Date
    , dateText : String
    , pickerModel : DatePicker.Model
    }
```

To get a `DatePicker.model` use the `DatePicker.init` function. If you want the current day to be highlighted, you have to set it using `DatePicker.setToday` or use `DatePicker.initWithToday` 

```elm
init : ( Model, Cmd Msg )
init =
    ( { date = Nothing
      , dateText = ""
      , pickerModel = DatePicker.init
      }
    , Task.perform SetToday Date.today
    )


update : Msg -> Model -> ( Model, Cmd Msg )
...
    SetToday today ->
    ( { model
        | pickerModel =
            model.pickerModel
                |> DatePicker.setToday today
        }
    , Cmd.none
    )
...
```

To display the date picker use the `DatePicker.input` function:

```elm
view : Model -> Html Msg
view model =
    Element.layout [ Element.width Element.shrink] <|
        DatePicker.input [Element.centerX, Element.centerY ]
            { onChange = ChangePicker
            , selected = model.date
            , text = model.dateText
            , label =
                Input.labelAbove [] <|
                    Element.text "Pick A Date"
            , placeholder = Nothing
            , settings = DatePicker.defaultSettings
            , model = model.pickerModel
            }
```
You have to handle both changes to the text and selection of a date:

```elm
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
                      }
                    , Cmd.none
                    )
...
```

`DatePicker.defaultSettings` is a reasonable start, just change what you need.

```elm
settings : DatePicker.Settings msg
settings =
    let
        default =
            DatePicker.defaultSettings
    in
    { default
        | firstDayOfWeek = Sun
        , disabled =
            \day ->
                Date.weekday day == Sun
    }
```