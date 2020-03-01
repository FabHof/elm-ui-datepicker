module Internal.TestHelper exposing
    ( calendarAttr
    , calendarAttrHtml
    , dayInMonthAttr
    , dayInMonthAttrHtml
    , inputAttr
    , inputAttrHtml
    , nextMonthAttr
    , nextMonthAttrHtml
    , previousMonthAttr
    , previousMonthAttrHtml
    , selectedAttr
    , selectedAttrHtml
    , tableAttr
    , tableAttrHtml
    , todayAttr
    , todayAttrHtml
    )

import Element exposing (Attribute)
import Html
import Html.Attributes


inputAttr : Attribute msg
inputAttr =
    inputAttrHtml
        |> Element.htmlAttribute


inputAttrHtml : Html.Attribute msg
inputAttrHtml =
    testAttribute "input"


calendarAttr : Attribute msg
calendarAttr =
    calendarAttrHtml
        |> Element.htmlAttribute


calendarAttrHtml : Html.Attribute msg
calendarAttrHtml =
    testAttribute "calendar"


tableAttr : Attribute msg
tableAttr =
    tableAttrHtml
        |> Element.htmlAttribute


tableAttrHtml : Html.Attribute msg
tableAttrHtml =
    testAttribute "table"


nextMonthAttr : Attribute msg
nextMonthAttr =
    nextMonthAttrHtml
        |> Element.htmlAttribute


nextMonthAttrHtml : Html.Attribute msg
nextMonthAttrHtml =
    testAttribute "nextMonth"


previousMonthAttr : Attribute msg
previousMonthAttr =
    previousMonthAttrHtml
        |> Element.htmlAttribute


previousMonthAttrHtml : Html.Attribute msg
previousMonthAttrHtml =
    testAttribute "previousMonth"


dayInMonthAttr : Attribute msg
dayInMonthAttr =
    dayInMonthAttrHtml
        |> Element.htmlAttribute


dayInMonthAttrHtml : Html.Attribute msg
dayInMonthAttrHtml =
    testAttribute "dayInMonth"


todayAttr : Attribute msg
todayAttr =
    todayAttrHtml
        |> Element.htmlAttribute


todayAttrHtml : Html.Attribute msg
todayAttrHtml =
    Html.Attributes.attribute "elm-test-alt" "today"


selectedAttr : Attribute msg
selectedAttr =
    selectedAttrHtml
        |> Element.htmlAttribute


selectedAttrHtml : Html.Attribute msg
selectedAttrHtml =
    testAttribute "selected"


testAttribute : String -> Html.Attribute msg
testAttribute name =
    Html.Attributes.attribute "elm-test" name
