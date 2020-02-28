module Internal.TestHelper exposing (calendarAttr, calendarAttrHtml, inputAttr, inputAttrHtml)

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
