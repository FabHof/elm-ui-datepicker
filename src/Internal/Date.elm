module Internal.Date exposing (formatMaybeLanguage)

import Date exposing (Date, Language)


formatMaybeLanguage : Maybe Language -> String -> Date -> String
formatMaybeLanguage maybeLanguage string =
    case maybeLanguage of
        Just language ->
            Date.formatWithLanguage language string

        Nothing ->
            Date.format string