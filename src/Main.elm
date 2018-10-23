module Main exposing (Log, LogLevel(..), Model, Msg(..), decodeLog, decodeLogLevel, decodeLogs, fetchLogs, filterLogs, init, levelToString, main, subscriptions, update, view, viewFilterRadio, viewFilters, viewKeyedLog, viewLog, viewLogs)

import Browser
import Html exposing (Html, div, form, input, label, table, tbody, td, text, tr)
import Html.Attributes exposing (checked, class, for, id, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Http


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type Msg
    = FetchLogs
    | LoadLogs (Result Http.Error String)
    | Filter String
    | FilterLevel LogLevel Bool


type LogLevel
    = Info
    | Warning
    | Error


levelToString level =
    case level of
        Error ->
            "error"

        Warning ->
            "warning"

        Info ->
            "info"


type alias Log =
    { number : Int
    , content : String
    , level : LogLevel
    }


type alias Model =
    { logs : List Log
    , filter : String
    , filterLevels : List LogLevel
    , url : String
    }


init : Maybe String -> ( Model, Cmd Msg )
init maybeUrl =
    let
        url =
            Maybe.withDefault "http://localhost:8080/logs" maybeUrl
    in
    ( { logs = []
      , filter = ""
      , filterLevels = [ Info ]
      , url = url
      }
    , fetchLogs url
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchLogs ->
            ( model, fetchLogs model.url )

        LoadLogs (Ok logs) ->
            ( { model | logs = decodeLogs logs }, Cmd.none )

        LoadLogs (Err _) ->
            ( model, Cmd.none )

        Filter filter ->
            ( { model | filter = filter }, Cmd.none )

        FilterLevel level checked ->
            case checked of
                True ->
                    ( { model | filterLevels = level :: model.filterLevels }, Cmd.none )

                False ->
                    ( { model | filterLevels = List.filter (\l -> l /= level) model.filterLevels }
                    , Cmd.none
                    )


decodeLogs : String -> List Log
decodeLogs logs =
    String.lines logs
        |> List.indexedMap decodeLog


decodeLog index log =
    { number = index, content = log, level = decodeLogLevel log }


decodeLogLevel log =
    if String.contains "error" (String.toLower log) then
        Error

    else if String.contains "warn" (String.toLower log) then
        Warning

    else
        Info


view : Model -> Html Msg
view model =
    div [ class "logs" ]
        [ lazy2 viewFilters model.filterLevels model.filter
        , lazy3 viewLogs model.filter model.filterLevels model.logs
        ]


viewFilters : List LogLevel -> String -> Html Msg
viewFilters filterLevels filter =
    form [ class "filters" ]
        [ viewFilterRadio Error filterLevels
        , viewFilterRadio Warning filterLevels
        , viewFilterRadio Info filterLevels
        , input [ onInput (\f -> Filter f), value filter ] []
        ]


viewFilterRadio : LogLevel -> List LogLevel -> Html Msg
viewFilterRadio level filterLevels =
    div []
        [ input
            [ type_ "checkbox"
            , id (levelToString level)
            , value (levelToString level)
            , checked (List.member level filterLevels)
            , onCheck (\checked -> FilterLevel level checked)
            ]
            []
        , label [ for (levelToString level) ] [ text (levelToString level) ]
        ]


viewLogs : String -> List LogLevel -> List Log -> Html Msg
viewLogs filter filterLevels logs =
    table [] [ Keyed.node "tbody" [] (List.map viewKeyedLog (filterLogs filter filterLevels logs)) ]


filterLogs : String -> List LogLevel -> List Log -> List Log
filterLogs filter filterLevels logs =
    logs
        |> List.filter (\s -> List.member s.level filterLevels)
        |> List.filter (\s -> String.contains (String.toLower filter) (String.toLower s.content))


viewKeyedLog : Log -> ( String, Html Msg )
viewKeyedLog log =
    ( String.fromInt log.number, lazy viewLog log )


viewLog : Log -> Html Msg
viewLog log =
    tr [ class ("level-" ++ levelToString log.level) ]
        [ td [] [ text (String.fromInt log.number) ]
        , td [] [ text log.content ]
        ]


fetchLogs : String -> Cmd Msg
fetchLogs url =
    Http.send LoadLogs <|
        Http.getString "http://localhost:8080/logs"
