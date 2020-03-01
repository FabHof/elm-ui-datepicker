module PlaygroundSpec exposing (..)

import Runner
import Spec exposing (..)
import Spec.Claim as Claim
import Spec.Markup as Markup
import Spec.Markup.Event as Event
import Spec.Markup.Selector exposing (..)
import Wrapper


clickSpec : Spec Wrapper.Model Wrapper.Msg
clickSpec =
    describe "simple date picker"
        [ scenario "input clicked"
            (given Wrapper.simpleApp
                |> when "the input field is clicked"
                    [ Markup.target << by [ tag "input" ]
                    , Event.click
                    ]
                |> it "is visible"
                    (Markup.observeElement
                        |> Markup.query
                        << by [ attribute ( "elm-test", "calendar" ) ]
                        |> expect Claim.isSomething
                    )
            )
        ]


main =
    Runner.browserProgram
        [ clickSpec
        ]
