module TodoList exposing (..)

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (onInput, onClick)
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)
import Http


host : String
host =
    "http://localhost:3000/tareas2"


main : Program Never Model Msg
main =
    Html.program
        { init = update ShowList init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Tarea =
    { ide : Int
    , tarea : String
    , estado : Bool
    }


type alias Model =
    { task : String
    , state : Bool
    , tareas : List Tarea
    , currentView : CurrentView
    }


type CurrentView
    = OnlyView
    | ErrorView1 String
    | ErrorView2 String


init : Model
init =
    { task = ""
    , state = False
    , tareas = []
    , currentView = OnlyView
    }


decodeTarea : Decoder (List Tarea)
decodeTarea =
    Decode.list decTarea


decTarea : Decode.Decoder Tarea
decTarea =
    Decode.map3 Tarea
        (Decode.field "id" Decode.int)
        (Decode.field "tarea" Decode.string)
        (Decode.field "estado" Decode.bool)


tareaEncoder : Tarea -> Encode.Value
tareaEncoder tar =
    Encode.object
        [ ( "tarea", Encode.string tar.tarea )
        , ( "estado", Encode.bool tar.estado )
        ]


getTareas : Http.Request (List Tarea)
getTareas =
    Http.get (host ++ "?order=id") decodeTarea


postTarea : Tarea -> Http.Request ()
postTarea tar =
    Http.request
        { method = "POST"
        , headers = []
        , url = host
        , body = Http.jsonBody (tareaEncoder tar)
        , expect = Http.expectStringResponse (\_ -> Ok ())
        , timeout = Nothing
        , withCredentials = False
        }


patchTarea : Tarea -> Http.Request ()
patchTarea tar =
    Http.request
        { method = "PATCH"
        , headers = []
        , url = host ++ "?id=eq." ++ (toString tar.ide)
        , body = Http.jsonBody (tareaEncoder tar)
        , expect = Http.expectStringResponse (\_ -> Ok ())
        , timeout = Nothing
        , withCredentials = False
        }



-- UPDATE


type Msg
    = Task String
    | ShowList
    | Submit
    | TaskRespPost (Result Http.Error ())
    | TaskRespGet (Result Http.Error (List Tarea))
    | ToggleTask Tarea


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowList ->
            ( model
            , Http.send TaskRespGet (getTareas)
            )

        Task tarea ->
            ( { model | task = tarea }, Cmd.none )

        Submit ->
            ( model
            , Http.send TaskRespPost (postTarea (Tarea 0 model.task model.state))
            )

        TaskRespPost (Ok ()) ->
            update ShowList { model | task = "" }

        TaskRespPost (Err error) ->
            ( { model | currentView = ErrorView1 (toString error) }, Cmd.none )

        TaskRespGet (Ok ls) ->
            ( { model | currentView = OnlyView, tareas = ls }
            , Cmd.none
            )

        TaskRespGet (Err error) ->
            ( { model | currentView = ErrorView2 (toString error) }, Cmd.none )

        ToggleTask tar ->
            let
                tarea =
                    { tar | estado = not tar.estado }
            in
                ( model
                , Http.send TaskRespPost
                    (patchTarea tarea)
                )



-- VIEW


view : Model -> Html Msg
view model =
    case model.currentView of
        OnlyView ->
            onlyView model

        ErrorView1 error ->
            errorView1 error

        ErrorView2 error ->
            errorView2 error


errorView2 : String -> Html Msg
errorView2 error =
    div [] [ text error ]


errorView1 : String -> Html Msg
errorView1 error =
    div [] [ text error ]


onlyView : Model -> Html Msg
onlyView model =
    div []
        [ div []
            [ Html.ul []
                (List.map
                    (\tar ->
                        Html.li
                            [ style
                                [ ( "font-family", "Verdana" ) ]
                            ]
                            [ text (tar.tarea ++ "  "), checkbox (ToggleTask tar) "" tar.estado ]
                    )
                    model.tareas
                )
            ]
        , input
            [ type_ "text"
            , placeholder "Task"
            , onInput Task
            , Attr.value model.task
            ]
            []
        , div []
            [ button [ onClick Submit, myStyle ]
                [ text "Submit Task" ]
            , div
                []
                [ text <| toString model.state ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


checkbox : msg -> String -> Bool -> Html msg
checkbox msg name bool =
    label
        [ style [ ( "padding", "10px" ) ]
        ]
        [ input [ type_ "checkbox", checked bool, onClick msg ] []
        , text name
        ]


myStyle : Attribute msg
myStyle =
    style
        [ ( "width", "100px" )
        , ( "height", "40px" )
        , ( "padding", "10px 0" )
        , ( "font-size", "1em" )
        , ( "text-align", "center" )
        ]
