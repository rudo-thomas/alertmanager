module Utils.Filter exposing
    ( Filter
    , MatchOperator(..)
    , Matcher
    , SilenceFormGetParams
    , convertFilterMatcher
    , emptySilenceFormGetParams
    , fromApiMatcher
    , generateAPIQueryString
    , nullFilter
    , parseFilter
    , parseGroup
    , parseMatcher
    , silencePreviewFilter
    , stringifyFilter
    , stringifyGroup
    , stringifyMatcher
    , toApiMatcher
    , toUrl
    , withMatchers
    )

import Char
import Data.Matcher
import Hex
import Json.Encode as Encode
import Parser exposing ((|.), (|=), Parser, Trailing(..))
import Set
import Url exposing (percentEncode)


type alias Filter =
    { text : Maybe String
    , group : Maybe String
    , customGrouping : Bool
    , receiver : Maybe String
    , showSilenced : Maybe Bool
    , showInhibited : Maybe Bool
    , showMuted : Maybe Bool
    , showActive : Maybe Bool
    }


nullFilter : Filter
nullFilter =
    { text = Nothing
    , group = Nothing
    , customGrouping = False
    , receiver = Nothing
    , showSilenced = Nothing
    , showInhibited = Nothing
    , showMuted = Nothing
    , showActive = Nothing
    }


generateQueryParam : String -> Maybe String -> Maybe String
generateQueryParam name =
    Maybe.map (percentEncode >> (++) (name ++ "="))


toUrl : String -> Filter -> String
toUrl baseUrl { receiver, customGrouping, showSilenced, showInhibited, showMuted, showActive, text, group } =
    let
        parts =
            [ ( "silenced", Maybe.withDefault False showSilenced |> boolToString |> Just )
            , ( "inhibited", Maybe.withDefault False showInhibited |> boolToString |> Just )
            , ( "muted", Maybe.withDefault False showMuted |> boolToString |> Just )
            , ( "active", Maybe.withDefault True showActive |> boolToString |> Just )
            , ( "filter", emptyToNothing text )
            , ( "receiver", emptyToNothing receiver )
            , ( "group", group )
            , ( "customGrouping", boolToMaybeString customGrouping )
            ]
                |> List.filterMap (\( a, b ) -> generateQueryParam a b)
    in
    if List.length parts > 0 then
        baseUrl
            ++ (parts
                    |> String.join "&"
                    |> (++) "?"
               )

    else
        baseUrl


generateAPIQueryString : Filter -> String
generateAPIQueryString { receiver, showSilenced, showInhibited, showMuted, showActive, text, group } =
    let
        filter_ =
            case parseFilter (Maybe.withDefault "" text) of
                Just matchers_ ->
                    List.map (stringifyMatcher >> Just >> Tuple.pair "filter") matchers_

                Nothing ->
                    []

        parts =
            filter_
                ++ [ ( "silenced", Maybe.withDefault False showSilenced |> boolToString |> Just )
                   , ( "inhibited", Maybe.withDefault False showInhibited |> boolToString |> Just )
                   , ( "muted", Maybe.withDefault False showMuted |> boolToString |> Just )
                   , ( "active", Maybe.withDefault True showActive |> boolToString |> Just )
                   , ( "receiver", emptyToNothing receiver )
                   , ( "group", group )
                   ]
                |> List.filterMap (\( a, b ) -> generateQueryParam a b)
    in
    if List.length parts > 0 then
        parts
            |> String.join "&"
            |> (++) "?"

    else
        ""


boolToMaybeString : Bool -> Maybe String
boolToMaybeString b =
    if b then
        Just "true"

    else
        Nothing


boolToString : Bool -> String
boolToString b =
    if b then
        "true"

    else
        "false"


emptyToNothing : Maybe String -> Maybe String
emptyToNothing str =
    case str of
        Just "" ->
            Nothing

        _ ->
            str


type alias Matcher =
    { key : String
    , op : MatchOperator
    , value : String
    }


toApiMatcher : Matcher -> Data.Matcher.Matcher
toApiMatcher { key, op, value } =
    let
        ( isRegex, isEqual ) =
            case op of
                Eq ->
                    ( False, True )

                NotEq ->
                    ( False, False )

                RegexMatch ->
                    ( True, True )

                NotRegexMatch ->
                    ( True, False )
    in
    { name = key
    , isRegex = isRegex
    , isEqual = Just isEqual
    , value = value
    }


fromApiMatcher : Data.Matcher.Matcher -> Matcher
fromApiMatcher { name, value, isRegex, isEqual } =
    let
        isEqualValue =
            case isEqual of
                Nothing ->
                    True

                Just justIsEqual ->
                    justIsEqual

        op =
            if not isRegex && isEqualValue then
                Eq

            else if not isRegex && not isEqualValue then
                NotEq

            else if isRegex && isEqualValue then
                RegexMatch

            else
                NotRegexMatch
    in
    { key = name
    , value = value
    , op = op
    }


type MatchOperator
    = Eq
    | NotEq
    | RegexMatch
    | NotRegexMatch


matchers : List ( String, MatchOperator )
matchers =
    [ ( "=~", RegexMatch )
    , ( "!~", NotRegexMatch )
    , ( "=", Eq )
    , ( "!=", NotEq )
    ]


parseFilter : String -> Maybe (List Matcher)
parseFilter =
    Parser.run filter
        >> Result.toMaybe


parseMatcher : String -> Maybe Matcher
parseMatcher =
    Parser.run matcher
        >> Result.toMaybe


stringifyGroup : List String -> Maybe String
stringifyGroup list =
    if List.isEmpty list then
        Just ""

    else if list == [ "alertname" ] then
        Nothing

    else
        Just (String.join "," list)


parseGroup : Maybe String -> List String
parseGroup maybeGroup =
    case maybeGroup of
        Nothing ->
            [ "alertname" ]

        Just something ->
            String.split "," something
                |> List.filter (String.length >> (<) 0)


stringifyFilter : List Matcher -> String
stringifyFilter matchers_ =
    case matchers_ of
        [] ->
            ""

        list ->
            (list
                |> List.map stringifyMatcher
                |> String.join ", "
                |> (++) "{"
            )
                ++ "}"


stringifyMatcher : Matcher -> String
stringifyMatcher { key, op, value } =
    key
        ++ (matchers
                |> List.filter (Tuple.second >> (==) op)
                |> List.head
                |> Maybe.map Tuple.first
                |> Maybe.withDefault ""
           )
        ++ Encode.encode 0 (Encode.string value)


convertFilterMatcher : Matcher -> Data.Matcher.Matcher
convertFilterMatcher { key, op, value } =
    { name = key
    , value = value
    , isRegex = (op == RegexMatch) || (op == NotRegexMatch)
    , isEqual = Just ((op == Eq) || (op == RegexMatch))
    }


filter : Parser (List Matcher)
filter =
    Parser.succeed identity
        |= Parser.sequence
            { start = "{"
            , separator = ","
            , end = "}"
            , spaces = Parser.spaces
            , item = item
            , trailing = Forbidden
            }
        |. Parser.end


matcher : Parser Matcher
matcher =
    Parser.succeed identity
        |. Parser.spaces
        |= item
        |. Parser.spaces
        |. Parser.end


item : Parser Matcher
item =
    Parser.succeed Matcher
        |= Parser.variable
            { start = isVarChar
            , inner = isVarChar
            , reserved = Set.empty
            }
        |= (matchers
                |> List.map
                    (\( keyword, matcher_ ) ->
                        Parser.succeed matcher_
                            |. Parser.keyword keyword
                    )
                |> Parser.oneOf
           )
        |= string '"'


type alias StringHelpState =
    { separator : Char
    , chars : List Char
    }


string : Char -> Parser String
string separator =
    Parser.succeed identity
        |. Parser.token (String.fromChar separator)
        |= Parser.loop (StringHelpState separator []) stringHelp
        |> Parser.andThen
            (\ms ->
                case ms of
                    Just s ->
                        Parser.succeed s

                    Nothing ->
                        Parser.problem "failed to parse string literal"
            )



{- All unescaping is based on https://go.dev/ref/spec#Rune_literals because PromQL and hence Alertmanager use the Go escaping rules -}


unescapeOctal : String -> Maybe Char
unescapeOctal s =
    String.toList s
        |> List.map (\ch -> Char.toCode ch - Char.toCode '0')
        |> List.foldl
            (\digit ->
                Maybe.andThen
                    (\i ->
                        if digit >= 0 && digit < 8 then
                            Just (i * 8 + digit)

                        else
                            Nothing
                    )
            )
            (Just 0)
        |> Maybe.map Char.fromCode


unescapeHex : String -> Maybe Char
unescapeHex s =
    String.toLower s
        |> Hex.fromString
        |> Result.toMaybe
        |> Maybe.map Char.fromCode


escapes : List ( String, Char )
escapes =
    [ ( "\\a", '\u{0007}' )
    , ( "\\b", '\u{0008}' )
    , ( "\\f", '\u{000C}' )
    , ( "\\n", '\n' )
    , ( "\\r", '\u{000D}' )
    , ( "\\t", '\t' )
    , ( "\\v", '\u{000B}' )
    , ( "\\\\", '\\' )
    , ( "\\'", '\'' )
    , ( "\\\"", '"' )
    ]


stateAppendMappedChompedString : StringHelpState -> (String -> Maybe Char) -> Parser () -> Parser (Parser.Step StringHelpState (Maybe String))
stateAppendMappedChompedString state func parser =
    Parser.getChompedString parser
        |> Parser.map func
        |> Parser.map
            (\mch ->
                case mch of
                    Just ch ->
                        Parser.Loop { state | chars = state.chars ++ [ ch ] }

                    Nothing ->
                        Parser.Done Nothing
            )


stringHelp : StringHelpState -> Parser (Parser.Step StringHelpState (Maybe String))
stringHelp state =
    Parser.oneOf
        [ Parser.succeed (Parser.Done (Just (String.fromList state.chars)))
            |. Parser.token (String.fromChar state.separator)
        , Parser.succeed ()
            |. Parser.token "\\x"
            |. Parser.chompIf Char.isHexDigit
            |. Parser.chompIf Char.isHexDigit
            |> stateAppendMappedChompedString state (String.dropLeft 2 >> unescapeHex)
        , Parser.succeed ()
            |. Parser.token "\\u"
            |. Parser.chompIf Char.isHexDigit
            |. Parser.chompIf Char.isHexDigit
            |. Parser.chompIf Char.isHexDigit
            |. Parser.chompIf Char.isHexDigit
            |> stateAppendMappedChompedString state (String.dropLeft 2 >> unescapeHex)
        , Parser.succeed ()
            |. Parser.token "\\U"
            |. Parser.chompIf Char.isHexDigit
            |. Parser.chompIf Char.isHexDigit
            |. Parser.chompIf Char.isHexDigit
            |. Parser.chompIf Char.isHexDigit
            |. Parser.chompIf Char.isHexDigit
            |. Parser.chompIf Char.isHexDigit
            |. Parser.chompIf Char.isHexDigit
            |. Parser.chompIf Char.isHexDigit
            |> stateAppendMappedChompedString state (String.dropLeft 2 >> unescapeHex)
        , escapes
            |> List.map
                (\( escapeSequence, char ) ->
                    Parser.succeed char
                        |. Parser.token escapeSequence
                )
            |> Parser.oneOf
            |> Parser.map (\ch -> Parser.Loop { state | chars = state.chars ++ [ ch ] })
        , Parser.succeed ()
            |. Parser.token "\\"
            |. Parser.chompIf Char.isOctDigit
            |. Parser.chompIf Char.isOctDigit
            |. Parser.chompIf Char.isOctDigit
            |> stateAppendMappedChompedString state (String.dropLeft 1 >> unescapeOctal)
        , Parser.problem "unsupported escape sequence"
            |. Parser.token "\\"
        , Parser.succeed ()
            |. Parser.chompIf (\_ -> True)
            |> stateAppendMappedChompedString state (String.toList >> List.head)
        ]


isVarChar : Char -> Bool
isVarChar char =
    Char.isLower char
        || Char.isUpper char
        || (char == '_')
        || Char.isDigit char


withMatchers : List Matcher -> Filter -> Filter
withMatchers matchers_ filter_ =
    { filter_ | text = Just (stringifyFilter matchers_) }


silencePreviewFilter : List Data.Matcher.Matcher -> Filter
silencePreviewFilter apiMatchers =
    { nullFilter
        | text =
            List.map fromApiMatcher apiMatchers
                |> stringifyFilter
                |> Just
        , showSilenced = Just True
        , showInhibited = Just True
        , showMuted = Just True
        , showActive = Just True
    }


type alias SilenceFormGetParams =
    { matchers : List Matcher
    , comment : String
    }


emptySilenceFormGetParams : SilenceFormGetParams
emptySilenceFormGetParams =
    { matchers = []
    , comment = ""
    }
