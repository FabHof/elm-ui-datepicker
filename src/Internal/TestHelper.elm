module Internal.TestHelper exposing (calendarAttr, calendarAttrHtml, inputAttr, inputAttrHtml, nextMonthAttr, nextMonthAttrHtml, previousMonthAttr, previousMonthAttrHtml)

import Element exposing (Attribute)
import Html
import Html.Attributes


calendarAttr : Attribute msg
calendarAttr =
    calendarAttrHtml
        |> Element.htmlAttribute


calendarAttrHtml : Html.Attribute msg
calendarAttrHtml =
    testAttribute "calendar"


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


inputAttr : Attribute msg
inputAttr =
    inputAttrHtml
        |> Element.htmlAttribute


inputAttrHtml : Html.Attribute msg
inputAttrHtml =
    testAttribute "input"


testAttribute : String -> Html.Attribute msg
testAttribute name =
    Html.Attributes.attribute "elm-test" name
