module Main exposing (main)

import Attr
import Browser
import Browser.Events exposing (onKeyPress)
import Color exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Html
import Http
import Json.Decode
import Json.Encode
import PlanParsers.Json exposing (..)
import PlanTree
import Ports exposing (..)
import Time


type Page
    = DisplayPage
    | InputPage
    | LoginPage
    | SavedPlansPage


type Msg
    = ChangePassword String
    | ChangeUserName String
    | ChangeEmail String
    | ChangePlanText String
    | CreatePlan
    | DumpModel ()
    | FinishLogin (Result Http.Error String)
    | FinishSavedPlans (Result Http.Error (List SavedPlan))
    | MouseEnteredPlanNode Plan
    | MouseLeftPlanNode Plan
    | NoOp
    | RequestLogin
    | RequestLogout
    | RequestSavedPlans
    | SendHeartbeat Time.Posix
    | StartLogin
    | ShowPlan String
    | SubmitPlan
    | ToggleMenu


type alias Model =
    { currPage : Page
    , currPlanText : String
    , email : String
    , isMenuOpen : Bool
    , lastError : String
    , password : String
    , savedPlans : List SavedPlan
    , selectedNode : Maybe Plan
    , sessionId : Maybe String
    , username : String
    }


type alias Flags =
    { sessionId : Maybe String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { currPage = InputPage
      , currPlanText = ""
      , email = ""
      , isMenuOpen = False
      , lastError = ""
      , password = ""
      , savedPlans = []
      , selectedNode = Nothing
      , sessionId = flags.sessionId
      , username = ""
      }
    , Cmd.none
    )


serverUrl =
    "http://localhost:3001/"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ dumpModel DumpModel
        , Time.every (100 * 1000) SendHeartbeat
        , onKeyPress <| keyDecoder model
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangePassword s ->
            ( { model | password = s }, Cmd.none )

        ChangeUserName s ->
            ( { model | username = s }, Cmd.none )

        ChangeEmail s ->
            ( { model | email = s }, Cmd.none )

        ChangePlanText s ->
            ( { model | currPlanText = s }, Cmd.none )

        CreatePlan ->
            ( { model | currPage = InputPage, currPlanText = "" }, Cmd.none )

        DumpModel () ->
            ( Debug.log "model" model, Cmd.none )

        FinishLogin (Ok sessionId) ->
            ( { model | sessionId = Just sessionId, currPage = InputPage }
            , saveSessionId <| Just sessionId
            )

        FinishLogin (Err error) ->
            ( { model | lastError = httpErrorString error }, Cmd.none )

        FinishSavedPlans (Ok savedPlans) ->
            ( { model | savedPlans = savedPlans }, Cmd.none )

        FinishSavedPlans (Err error) ->
            ( { model | lastError = httpErrorString error }, Cmd.none )

        MouseEnteredPlanNode plan ->
            ( { model | selectedNode = Just plan }, Cmd.none )

        MouseLeftPlanNode plan ->
            ( { model | selectedNode = Nothing }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        RequestLogin ->
            ( { model | currPage = LoginPage, password = "", username = "", email = "" }, Cmd.none )

        -- TODO fix this
        RequestLogout ->
            ( model, Cmd.none )

        RequestSavedPlans ->
            ( { model | currPage = SavedPlansPage }
            , getSavedPlans model.sessionId
            )

        SendHeartbeat _ ->
            ( model, sendHeartbeat model.sessionId )

        ShowPlan planText ->
            ( { model | currPlanText = planText, currPage = DisplayPage }
            , Cmd.none
            )

        StartLogin ->
            ( model, login model.username model.password )

        SubmitPlan ->
            ( { model | currPage = DisplayPage }, Cmd.none )

        ToggleMenu ->
            ( { model | isMenuOpen = not model.isMenuOpen }, Cmd.none )



-- EFFECTS


login : String -> String -> Cmd Msg
login username password =
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


getSavedPlans : Maybe String -> Cmd Msg
getSavedPlans sessionId =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "SessionId" <| Maybe.withDefault "" sessionId ]
        , url = serverUrl ++ "plans"
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        , expect = Http.expectJson FinishSavedPlans decodeSavedPlans
        }


sendHeartbeat : Maybe String -> Cmd Msg
sendHeartbeat sessionId =
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


keyDecoder : Model -> Json.Decode.Decoder Msg
keyDecoder model =
    Json.Decode.map2 Tuple.pair
        (Json.Decode.field "altKey" Json.Decode.bool)
        (Json.Decode.field "shiftKey" Json.Decode.bool)
        |> Json.Decode.andThen
            (\altAndShiftFlags ->
                case altAndShiftFlags of
                    ( True, True ) ->
                        Json.Decode.field "code" Json.Decode.string
                            |> Json.Decode.map (keyToMsg model)

                    _ ->
                        Json.Decode.succeed NoOp
            )


keyToMsg : Model -> String -> Msg
keyToMsg model code =
    case ( code, model.sessionId ) of
        ( "KeyS", Just id ) ->
            RequestSavedPlans

        ( "KeyN", _ ) ->
            CreatePlan

        _ ->
            NoOp



-- FUNCTIONS


httpErrorString : Http.Error -> String
httpErrorString error =
    case error of
        Http.BadBody message ->
            "Unable to handle response: " ++ message

        Http.BadStatus statusCode ->
            "Server error: " ++ String.fromInt statusCode

        Http.BadUrl url ->
            "invalid URL: " ++ url

        Http.NetworkError ->
            "Network error"

        Http.Timeout ->
            "Request timeout"



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        content =
            case model.currPage of
                DisplayPage ->
                    displayPage model

                InputPage ->
                    inputPage model

                LoginPage ->
                    loginPage model

                SavedPlansPage ->
                    savedPlansPage model
    in
    { title = "VisExp"
    , body =
        [ layout [ inFront <| menuPanel model ] <|
            column [ width fill, spacingXY 0 20 ]
                [ navBar
                , content
                ]
        ]
    }


loginPage : Model -> Element Msg
loginPage model =
    column [ paddingXY 0 20, spacingXY 0 10, width (px 300), centerX ]
        [ Input.username Attr.input
            { onChange = ChangeUserName
            , text = model.username
            , label = Input.labelAbove [] <| text "User name:"
            , placeholder = Nothing
            }
        , Input.currentPassword Attr.input
            { onChange = ChangePassword
            , text = model.password
            , label = Input.labelAbove [] <| text "Password:"
            , placeholder = Nothing
            , show = False
            }
        , Input.email Attr.input
            { onChange = ChangeEmail
            , text = model.email
            , label = Input.labelAbove [] <| text "Email:"
            , placeholder = Nothing
            }
        , Input.button Attr.greenButton
            { onPress = Just StartLogin
            , label = el [ centerX ] <| text "Login"
            }
        , el Attr.error <| text model.lastError
        ]


displayPage : Model -> Element Msg
displayPage model =
    let
        planTreeConfig =
            { onMouseEnteredNode = MouseEnteredPlanNode
            , onMouseLeftNode = MouseLeftPlanNode
            }
    in
    case Json.Decode.decodeString decodePlanJson model.currPlanText of
        Ok planJson ->
            PlanTree.render planTreeConfig planJson model.selectedNode

        Err err ->
            el [] <| text <| Json.Decode.errorToString err


navBar : Element Msg
navBar =
    row
        [ width fill
        , paddingXY 60 10
        , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
        , Border.color blue
        ]
        [ el [ alignLeft ] <| text "VisExp"
        , Input.button (Attr.greyButton ++ [ padding 5, alignRight, width (px 80) ])
            { onPress = Just ToggleMenu
            , label = el [ centerX ] <| text "Menu"
            }
        ]


inputPage : Model -> Element Msg
inputPage model =
    column
        [ width (px 800)
        , spacingXY 0 10
        , centerX
        ]
        [ Input.multiline
            [ height (px 300)
            , Border.width 1
            , Border.rounded 3
            , Border.color lightCharcoal
            , padding 3
            ]
            { onChange = ChangePlanText
            , text = model.currPlanText
            , placeholder = Nothing
            , label =
                Input.labelAbove [] <|
                    text "Paste the EXPLAIN output in JSON format:"
            , spellcheck = False
            }
        , Input.button
            (Attr.greenButton ++ [ alignRight, width (px 200), height (px 40) ])
            { onPress = Just SubmitPlan
            , label = el [ centerX ] <| text "Go!"
            }
        ]


savedPlansPage : Model -> Element Msg
savedPlansPage model =
    let
        annotateVersionName name planVersion =
            { version = planVersion.version
            , planText = planVersion.planText
            , createdAt = planVersion.createdAt
            , name = name
            }

        annotateVersions savedPlan =
            List.map (annotateVersionName savedPlan.name) savedPlan.versions

        tableAttrs =
            [ width (px 800)
            , paddingEach { top = 10, bottom = 50, left = 10, right = 10 }
            , spacingXY 10 10
            , centerX
            ]

        headerAttrs =
            [ Font.bold
            , Background.color Color.lightGrey
            , Border.color Color.darkCharcoal
            , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
            , centerX
            ]
    in
    table tableAttrs
        { data = List.concatMap annotateVersions model.savedPlans
        , columns =
            [ { header = el headerAttrs <| text "Plan name"
              , width = fill
              , view =
                    \plan ->
                        el
                            [ Font.underline
                            , mouseOver [ Font.color lightCharcoal ]
                            , onClick <| ShowPlan plan.planText
                            ]
                        <|
                            text plan.name
              }
            , { header = el headerAttrs <| text "Creation time"
              , width = fill
              , view = .createdAt >> text
              }
            , { header = el headerAttrs <| text "Version"
              , width = fill
              , view = .version >> String.fromInt >> text
              }
            ]
        }


menuPanel : Model -> Element Msg
menuPanel model =
    let
        items =
            -- TODO: Fix this to emit the right actions
            [ el [ pointer, onClick CreatePlan ] <| text "New plan" ]
                ++ (case model.sessionId of
                        Just _ ->
                            [ el [ pointer, onClick RequestSavedPlans ] <|
                                text "Saved plans"
                            , el [ pointer, onClick RequestLogout ] <| text "Logout"
                            ]

                        Nothing ->
                            [ el [ pointer, onClick RequestLogin ] <| text "Login" ]
                   )

        panel =
            column
                [ Background.color Color.white
                , Border.widthEach { left = 1, right = 0, top = 0, bottom = 0 }
                , Border.color Color.grey
                , Border.shadow
                    { offset = ( 0, 0 )
                    , size = 1
                    , blur = 10
                    , color = Color.lightCharcoal
                    }
                , Font.bold
                , Font.color Color.darkCharcoal
                , Font.family [ Font.sansSerif ]
                , width <| fillPortion 1
                , height fill
                , paddingXY 20 20
                , spacingXY 0 20
                ]
                items

        overlay =
            el [ width <| fillPortion 4, height fill, onClick ToggleMenu ] none
    in
    if model.isMenuOpen then
        row [ width fill, height fill ] [ overlay, panel ]

    else
        none


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
