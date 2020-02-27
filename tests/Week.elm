module Week exposing (..)

import Expect
import Fuzz exposing (list, string)
import Internal.Week as Week
import Test exposing (..)



-- TESTS


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
    describe "Week.indexedMap"
        [ test "index and element match" <|
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
