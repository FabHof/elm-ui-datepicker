module Week exposing (suite)

import Date
import Expect
import Fuzz exposing (intRange, list, string)
import Internal.Week as Week
import Test exposing (..)
import Time



-- TESTS


suite : Test
suite =
    concat
        [ getDay
        , toList
        , indexedMap
        , calendarWeekDays
        , weeksInMonth
        ]


getDay : Test
getDay =
    describe "Week.getDay returns correct element"
        [ testGetDay 0 Week.Day0
        , testGetDay 1 Week.Day1
        , testGetDay 2 Week.Day2
        , testGetDay 3 Week.Day3
        , testGetDay 4 Week.Day4
        , testGetDay 5 Week.Day5
        , testGetDay 6 Week.Day6
        ]


toList : Test
toList =
    fuzz (list string) "Week.toList returns Week.fromListWithDefault" <|
        \fuzzlist ->
            let
                week =
                    Week.fromListWithDefault "foo" fuzzlist
            in
            Week.toList week
                |> List.take (List.length fuzzlist)
                |> Expect.equal (List.take 7 fuzzlist)


indexedMap : Test
indexedMap =
    test "Week.indexedMap index and element match" <|
        \_ ->
            let
                week =
                    List.range 0 6
                        |> List.map
                            intToIndex
                        |> Week.fromListWithDefault Week.Day0
            in
            Week.indexedMap (\i el -> i == el) week
                |> Week.toList
                |> Expect.equalLists (List.range 0 6 |> List.map (always True))


calendarWeekDays : Test
calendarWeekDays =
    describe "Week.calendarWeekDays"
        [ test "without language" <|
            \_ ->
                Week.calendarWeekDays Time.Wed Nothing
                    |> Week.toList
                    |> Expect.equalLists [ "We", "Th", "Fr", "Sa", "Su", "Mo", "Tu" ]
        , test "with Language" <|
            \_ ->
                let
                    weekdayName =
                        \weekday ->
                            case weekday of
                                Time.Mon ->
                                    "Montag"

                                Time.Tue ->
                                    "Dienstag"

                                Time.Wed ->
                                    "Mittwoch"

                                Time.Thu ->
                                    "Donnerstag"

                                Time.Fri ->
                                    "Freitag"

                                Time.Sat ->
                                    "Samstag"

                                Time.Sun ->
                                    "Sonntag"

                    language =
                        { monthName = always "xxx"
                        , monthNameShort = always "xxx"
                        , weekdayName = always "xxx"
                        , weekdayNameShort = weekdayName
                        , dayWithSuffix = always "xxx"
                        }
                in
                Week.calendarWeekDays Time.Mon (Just language)
                    |> Week.toList
                    |> Expect.equalLists [ "Mo", "Di", "Mi", "Do", "Fr", "Sa", "So" ]
        ]


weeksInMonth : Test
weeksInMonth =
    describe "Week.weeksInMonth"
        [ fuzz2 (intRange 1 31) (intRange 1900 2100) "same result for every day in the month" <|
            \fuzzDay fuzzYear ->
                let
                    weeks1 =
                        Week.weeksInMonth (Date.fromCalendarDate fuzzYear Time.Mar 1) Time.Mon

                    weeksFuzzed =
                        Week.weeksInMonth (Date.fromCalendarDate fuzzYear Time.Mar fuzzDay) Time.Mon
                in
                Expect.equalLists weeks1 weeksFuzzed
        , test "expected length for long month" <|
            \_ ->
                Week.weeksInMonth (Date.fromCalendarDate 2019 Time.Sep 1) Time.Mon
                    |> List.length
                    |> Expect.equal 6
        , test "expected length for short month" <|
            \_ ->
                Week.weeksInMonth (Date.fromCalendarDate 2021 Time.Feb 1) Time.Mon
                    |> List.length
                    |> Expect.equal 4
        , test "expected length for middle month" <|
            \_ ->
                Week.weeksInMonth (Date.fromCalendarDate 2020 Time.Apr 1) Time.Mon
                    |> List.length
                    |> Expect.equal 5
        
        , fuzz (intRange 1600 2400) "contains range of days" <|
            \fuzzYear ->
                let 
                    normalizedDays =  Week.weeksInMonth (Date.fromCalendarDate fuzzYear Time.Apr 1) Time.Mon
                        |> List.map Week.toList
                        |> List.concat
                        |> List.indexedMap (\index day -> Date.add Date.Days (index * -1) day)
                in 
                    normalizedDays
                    |> List.all (\day -> Just day == List.head normalizedDays)
                    |> Expect.true "sequence of days"
        ]



-- HELPERS


testGetDay : Int -> Week.Index -> Test
testGetDay intIndex dayIndex =
    fuzz2 (list string) string ("for element " ++ String.fromInt intIndex) <|
        \fuzzlist defaultString ->
            let
                week =
                    Week.fromListWithDefault defaultString fuzzlist
            in
            case fuzzlist |> List.drop intIndex |> List.head of
                Just item ->
                    Week.getDay dayIndex week
                        |> Expect.equal item

                Nothing ->
                    Week.getDay dayIndex week
                        |> Expect.equal defaultString


intToIndex : Int -> Week.Index
intToIndex i =
    case i of
        0 ->
            Week.Day0

        1 ->
            Week.Day1

        2 ->
            Week.Day2

        3 ->
            Week.Day3

        4 ->
            Week.Day4

        5 ->
            Week.Day5

        6 ->
            Week.Day6

        _ ->
            Week.Day0
