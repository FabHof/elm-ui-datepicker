module Internal.Week exposing (Index(..), Week, calendarWeekDays, fromListWithDefault, getDay, indexedMap, toList, weeksInMonth)

import Date exposing (Date, Language)
import Internal.Date as Date
import Time exposing (Month(..), Weekday(..))


type Week a
    = Week (WeekModel a)


type alias WeekModel a =
    { day0 : a
    , day1 : a
    , day2 : a
    , day3 : a
    , day4 : a
    , day5 : a
    , day6 : a
    }


getDay : Index -> Week a -> a
getDay index (Week week) =
    case index of
        Day0 ->
            week.day0

        Day1 ->
            week.day1

        Day2 ->
            week.day2

        Day3 ->
            week.day3

        Day4 ->
            week.day4

        Day5 ->
            week.day5

        Day6 ->
            week.day6


indexedMap : (Index -> a -> b) -> Week a -> Week b
indexedMap fn (Week week) =
    Week
        { day0 = fn Day0 week.day0
        , day1 = fn Day1 week.day1
        , day2 = fn Day2 week.day2
        , day3 = fn Day3 week.day3
        , day4 = fn Day4 week.day4
        , day5 = fn Day5 week.day5
        , day6 = fn Day6 week.day6
        }


addNextDay : ( a, List a, a -> b ) -> ( a, List a, b )
addNextDay ( default, days, fn ) =
    case days of
        day :: rest ->
            ( default, rest, fn day )

        [] ->
            ( default, [], fn default )


fromListWithDefault : a -> List a -> Week a
fromListWithDefault default items =
    let
        ( _, _, week ) =
            ( default, items, WeekModel )
                |> addNextDay
                |> addNextDay
                |> addNextDay
                |> addNextDay
                |> addNextDay
                |> addNextDay
                |> addNextDay
    in
    Week week


type Index
    = Day0
    | Day1
    | Day2
    | Day3
    | Day4
    | Day5
    | Day6


toList : Week a -> List a
toList (Week week) =
    [ week.day0
    , week.day1
    , week.day2
    , week.day3
    , week.day4
    , week.day5
    , week.day6
    ]


calendarWeekDays : Weekday -> Maybe Language -> Week String
calendarWeekDays firstDayOfWeek maybeLanguage =
    let
        startDay =
            Date.floor (weekdayToInterval firstDayOfWeek) (Date.fromCalendarDate 2020 Jan 1)

        days =
            Date.range Date.Day 1 startDay (Date.add Date.Days 7 startDay)
    in
    fromListWithDefault "X" (List.map (Date.formatMaybeLanguage maybeLanguage "EEEEEE") days)


weekdayToInterval : Weekday -> Date.Interval
weekdayToInterval weekday =
    case weekday of
        Mon ->
            Date.Monday

        Tue ->
            Date.Tuesday

        Wed ->
            Date.Wednesday

        Thu ->
            Date.Thursday

        Fri ->
            Date.Friday

        Sat ->
            Date.Saturday

        Sun ->
            Date.Sunday


weeksInMonth : Date -> Weekday -> List (Week Date)
weeksInMonth month firstDayOfWeek =
    let
        weekdayInterval =
            weekdayToInterval firstDayOfWeek

        firstOfMonth =
            Date.fromCalendarDate (Date.year month) (Date.month month) 1

        start =
            firstOfMonth
                |> Date.floor weekdayInterval

        end =
            Date.add Date.Months 1 firstOfMonth
                |> Date.ceiling weekdayInterval

        weekDays startDay =
            Date.range Date.Day 1 startDay (Date.add Date.Days 7 startDay)

        toWeek list =
            fromListWithDefault (Date.fromOrdinalDate 2020 1) list
    in
    Date.range Date.Day 7 start end
        |> List.map (weekDays >> toWeek)
