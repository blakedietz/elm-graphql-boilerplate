module Auth exposing (..)

import Http
import Json.Decode
import Json.Encode
import Ports exposing (..)
import Time
import Utils exposing (httpErrorString)


type Msg
    = ChangePassword String
    | ChangeUserName String
    | FinishLogin (Result Http.Error String)
    | NoOp
    | SendHeartbeat Time.Posix
    | StartLogin


type alias Model =
    { lastError : String
    , password : String
    , sessionId : Maybe String
    , username : String
    }


init : Maybe String -> Model
init sessionId =
    { lastError = ""
    , password = ""
    , sessionId = sessionId
    , username = ""
    }



-- EFFECTS


login : String -> String -> String -> Cmd Msg
login serverUrl username password =
    let
        body =
            Http.jsonBody <|
                Json.Encode.object
                    [ ( "userName", Json.Encode.string username )
                    , ( "password", Json.Encode.string password )
                    ]

        responseDecoder =
            Json.Decode.field "sessionId" Json.Decode.string
    in
    Http.post
        { url = serverUrl ++ "login"
        , body = body
        , expect = Http.expectJson FinishLogin responseDecoder
        }


sendHeartbeat : String -> Maybe String -> Cmd Msg
sendHeartbeat serverUrl sessionId =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "SessionId" <| Maybe.withDefault "" sessionId ]
        , url = serverUrl ++ "heartbeat"
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        , expect = Http.expectWhatever <| always NoOp
        }



-- UPDATE


update : String -> Msg -> Model -> ( Model, Cmd Msg )
update serverUrl msg model =
    case msg of
        ChangePassword s ->
            ( { model | password = s }, Cmd.none )

        ChangeUserName s ->
            ( { model | username = s }, Cmd.none )

        FinishLogin (Ok sessionId) ->
            ( { model | sessionId = Just sessionId, username = "", password = "" }
            , saveSessionId <| Just sessionId
            )

        FinishLogin (Err error) ->
            ( { model | lastError = httpErrorString error }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        SendHeartbeat _ ->
            ( model, sendHeartbeat serverUrl model.sessionId )

        StartLogin ->
            ( model, login serverUrl model.username model.password )
