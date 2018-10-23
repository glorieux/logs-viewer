module Main exposing (Log, LogLevel(..), Model, Msg(..), decodeLog, decodeLogLevel, decodeLogs, fetchLogs, filterLogs, init, levelToString, main, subscriptions, update, view, viewFilterRadio, viewFilters, viewKeyedLog, viewLog, viewLogs)

import Browser
import Html exposing (Html, button, div, form, h1, input, label, span, table, tbody, td, text, tr)
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


type alias Logs =
    List Log


type LogStatus
    = Loading
    | Success Logs
    | Failure Http.Error


type alias Model =
    { logs : LogStatus
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
    ( { logs = Loading
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
            ( { model | logs = Loading }, fetchLogs model.url )

        LoadLogs (Ok logs) ->
            ( { model | logs = Success (decodeLogs logs) }, Cmd.none )

        LoadLogs (Err error) ->
            ( { model | logs = Failure error }, Cmd.none )

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


decodeLogs : String -> Logs
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
    case model.logs of
        Loading ->
            h1 [ class "tfd-logs-viewer-empty" ] [ text "Loading" ]

        Success logs ->
            let
                filteredLogs =
                    filterLogs model.filter model.filterLevels logs
            in
            div [ class "logviewer" ]
                [ lazy3 viewFilters model logs filteredLogs
                , lazy viewLogs filteredLogs
                ]

        Failure error ->
            h1 [ class "tfd-logs-viewer-empty" ] [ text "Error loading logs" ]


viewFilters : Model -> Logs -> Logs -> Html Msg
viewFilters { filter, filterLevels } logs filteredLogs =
    form [ class "logviewer__toolbar" ]
        [ viewCount logs filteredLogs
        , div [ class "logviewer__toolbar__filters" ]
            [ viewFilterRadio Info filterLevels
            , viewFilterRadio Warning filterLevels
            , viewFilterRadio Error filterLevels
            ]
        , input [ onInput (\f -> Filter f), value filter ] []
        , button [ onClick FetchLogs, type_ "button" ] [ text "refresh" ]
        ]


viewCount : Logs -> Logs -> Html Msg
viewCount logs filteredLogs =
    let
        logsLength =
            List.length logs

        countLogsLength =
            String.fromInt logsLength ++ " " ++ pluralize "line" "lines" logsLength

        filteredLogsLength =
            List.length filteredLogs

        count =
            if logsLength /= filteredLogsLength then
                countLogsLength ++ " (" ++ (String.fromInt filteredLogsLength ++ " " ++ pluralize "match" "matches" filteredLogsLength) ++ ")"

            else
                countLogsLength
    in
    div []
        [ div
            [ class "logviewer__toolbar__count" ]
            [ text count ]
        ]


viewFilterRadio : LogLevel -> List LogLevel -> Html Msg
viewFilterRadio level filterLevels =
    div [ class "checkbox tc-toggle checkbox" ]
        [ label [ for (levelToString level) ]
            [ input
                [ type_ "checkbox"
                , id (levelToString level)
                , value (levelToString level)
                , checked (List.member level filterLevels)
                , onCheck (\checked -> FilterLevel level checked)
                ]
                []
            , span [] [ text (levelToString level) ]
            ]
        ]


viewLogs : Logs -> Html Msg
viewLogs filteredLogs =
    table [ class "logviewer__content" ] [ Keyed.node "tbody" [] (List.map viewKeyedLog filteredLogs) ]


filterLogs : String -> List LogLevel -> Logs -> Logs
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
        [ td [ class "logviewer__content__line__number" ] [ text (String.fromInt log.number) ]
        , td [ class "logviewer__content__line__content" ] [ text log.content ]
        ]


fetchLogs : String -> Cmd Msg
fetchLogs url =
    Http.send LoadLogs <|
        Http.getString url


pluralize : String -> String -> Int -> String
pluralize singular plural number =
    if number == 1 then
        singular

    else
        plural
