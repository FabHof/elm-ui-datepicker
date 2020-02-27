module Week exposing (..)

import Expect
import Fuzz exposing (list, string)
import Internal.Week as Week
import Test exposing (..)


getDay : Test
getDay =
    describe "Week.getDay returns correct element"
        [ fuzz2 (list string) string "for first element" <|
            \fuzzlist defaultString ->
                let
                    week =
                        Week.fromListWithDefault defaultString fuzzlist
                in
                case List.head fuzzlist of
                    Just item ->
                        Week.getDay Week.Day0 week
                            |> Expect.equal item

                    Nothing ->
                        Week.getDay Week.Day0 week
                            |> Expect.equal defaultString
        , fuzz2 (list string) string "for 2nd element" <|
            \fuzzlist defaultString ->
                let
                    week =
                        Week.fromListWithDefault defaultString fuzzlist
                in
                case fuzzlist |> List.drop 1 |> List.head of
                    Just item ->
                        Week.getDay Week.Day1 week
                            |> Expect.equal item

                    Nothing ->
                        Week.getDay Week.Day1 week
                            |> Expect.equal defaultString
        , fuzz2 (list string) string "for 3rd element" <|
            \fuzzlist defaultString ->
                let
                    week =
                        Week.fromListWithDefault defaultString fuzzlist
                in
                case fuzzlist |> List.drop 2 |> List.head of
                    Just item ->
                        Week.getDay Week.Day2 week
                            |> Expect.equal item

                    Nothing ->
                        Week.getDay Week.Day2 week
                            |> Expect.equal defaultString
        , fuzz2 (list string) string "for 4th element" <|
            \fuzzlist defaultString ->
                let
                    week =
                        Week.fromListWithDefault defaultString fuzzlist
                in
                case fuzzlist |> List.drop 3 |> List.head of
                    Just item ->
                        Week.getDay Week.Day3 week
                            |> Expect.equal item

                    Nothing ->
                        Week.getDay Week.Day3 week
                            |> Expect.equal defaultString
        , fuzz2 (list string) string "for 5th element" <|
            \fuzzlist defaultString ->
                let
                    week =
                        Week.fromListWithDefault defaultString fuzzlist
                in
                case fuzzlist |> List.drop 4 |> List.head of
                    Just item ->
                        Week.getDay Week.Day4 week
                            |> Expect.equal item

                    Nothing ->
                        Week.getDay Week.Day4 week
                            |> Expect.equal defaultString
        , fuzz2 (list string) string "for 6th element" <|
            \fuzzlist defaultString ->
                let
                    week =
                        Week.fromListWithDefault defaultString fuzzlist
                in
                case fuzzlist |> List.drop 5 |> List.head of
                    Just item ->
                        Week.getDay Week.Day5 week
                            |> Expect.equal item

                    Nothing ->
                        Week.getDay Week.Day5 week
                            |> Expect.equal defaultString
        , fuzz2 (list string) string "for 7th element" <|
            \fuzzlist defaultString ->
                let
                    week =
                        Week.fromListWithDefault defaultString fuzzlist
                in
                case fuzzlist |> List.drop 6 |> List.head of
                    Just item ->
                        Week.getDay Week.Day6 week
                            |> Expect.equal item

                    Nothing ->
                        Week.getDay Week.Day6 week
                            |> Expect.equal defaultString
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
                                (\i ->
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
                                )
                            |> Week.fromListWithDefault Week.Day0
                in
                Week.indexedMap (\i el -> i == el) week
                    |> Week.toList
                    |> Expect.equalLists (List.range 0 6 |> List.map (always True))
        ]
