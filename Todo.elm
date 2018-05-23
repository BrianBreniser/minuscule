port module Todo exposing (..)

{-| TodoMVC implemented in Elm, using plain HTML and CSS for rendering.

This application is broken up into four distinct parts:

  1. Model  - a full description of the application as data
  2. Update - a way to update the model based on user actions
  3. View   - a way to visualize our model with HTML

This program is not particularly large, so definitely see the following
document for notes on structuring more complex GUIs with Elm:
http://guide.elm-lang.org/architecture/
-}

import Dom
import Task
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2)
import Navigation exposing (Location)
import UrlParser exposing (Parser, top, oneOf, parseHash)
import String
import Todo.Task


-- MODEL
-- The full application state of our todo app.


type Visibility
    = All
    | Active
    | Completed


type alias Model =
    { tasks : List Todo.Task.Model
    , field : String
    , uid : Int
    , visibility : Visibility
    }


type alias Flags =
    Maybe (List Todo.Task.Model)


emptyModel : Model
emptyModel =
    { tasks = []
    , visibility = All
    , field = ""
    , uid = 0
    }



-- UPDATE
-- A description of the kinds of actions that can be performed on the model of
-- our application. See the following post for more info on this pattern and
-- some alternatives: http://guide.elm-lang.org/architecture/


type Msg
    = NoOp
    | UpdateField String
    | Add
    | UpdateTask ( Int, Todo.Task.Msg )
    | DeleteComplete
    | CheckAll Bool
    | ChangeVisibility Visibility
    | SetVisibility Location



-- How we update our Model on any given Message


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "MESSAGE: " msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateField str ->
            let
                newModel =
                    { model | field = str }
            in
                ( newModel, save model.tasks )

        Add ->
            let
                description =
                    String.trim model.field

                newModel =
                    if String.isEmpty description then
                        model
                    else
                        { model
                            | uid = model.uid + 1
                            , field = ""
                            , tasks = model.tasks ++ [ Todo.Task.init description model.uid ]
                        }
            in
                ( newModel, save newModel.tasks )

        UpdateTask ( id, taskMsg ) ->
            let
                updateTask t =
                    if t.id == id then
                        Todo.Task.update taskMsg t
                    else
                        Just t

                newModel =
                    { model | tasks = List.filterMap updateTask model.tasks }
            in
                case taskMsg of
                    Todo.Task.Focus elementId ->
                        newModel ! [ save newModel.tasks, focusTask elementId ]

                    _ ->
                        ( newModel, save newModel.tasks )

        DeleteComplete ->
            let
                newModel =
                    { model
                        | tasks = List.filter (not << .completed) model.tasks
                    }
            in
                ( newModel, save newModel.tasks )

        CheckAll bool ->
            let
                updateTask t =
                    { t | completed = bool }

                newModel =
                    { model | tasks = List.map updateTask model.tasks }
            in
                ( newModel, save newModel.tasks )

        ChangeVisibility visibility ->
            let
                newModel =
                    { model | visibility = visibility }
            in
                ( newModel, save model.tasks )

        SetVisibility location ->
            ( setVisibility location model, Cmd.none )


focusTask : String -> Cmd Msg
focusTask elementId =
    Task.attempt (\_ -> NoOp) (Dom.focus elementId)



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "todomvc-wrapper"

        -- , style [ ( "visibility", "hidden" ) ]
        ]
        [ section
            [ class "todoapp" ]
            [ lazy taskEntry model.field
            , lazy2 taskList model.visibility model.tasks
            , lazy2 controls model.visibility model.tasks
            ]
        , infoFooter
        ]


taskEntry : String -> Html Msg
taskEntry task =
    header
        [ class "header" ]
        [ h1 [] [ text "todos" ]
        , input
            [ class "new-todo"
            , placeholder "What needs to be done?"
            , autofocus True
            , value task
            , name "newTodo"
            , onInput UpdateField
            , Todo.Task.onFinish Add NoOp
            ]
            []
        ]


taskList : Visibility -> List Todo.Task.Model -> Html Msg
taskList visibility tasks =
    let
        isVisible todo =
            case visibility of
                Completed ->
                    todo.completed

                Active ->
                    not todo.completed

                All ->
                    True

        allCompleted =
            List.all .completed tasks

        cssVisibility =
            if List.isEmpty tasks then
                "hidden"
            else
                "visible"
    in
        section
            [ class "main"
            , style [ ( "visibility", cssVisibility ) ]
            ]
            [ input
                [ class "toggle-all"
                , type_ "checkbox"
                , name "toggle"
                , checked allCompleted
                , onClick (CheckAll (not allCompleted))
                ]
                []
            , label
                [ for "toggle-all" ]
                [ text "Mark all as complete" ]
            , ul
                [ class "todo-list" ]
                (List.map
                    (\task ->
                        let
                            id =
                                task.id

                            taskView =
                                Todo.Task.view task
                        in
                            Html.map (\msg -> UpdateTask ( id, msg )) taskView
                    )
                    (List.filter isVisible tasks)
                )
            ]


controls : Visibility -> List Todo.Task.Model -> Html Msg
controls visibility tasks =
    let
        tasksCompleted =
            List.length (List.filter .completed tasks)

        tasksLeft =
            List.length tasks - tasksCompleted

        item_ =
            if tasksLeft == 1 then
                " item"
            else
                " items"
    in
        footer
            [ class "footer"
            , hidden (List.isEmpty tasks)
            ]
            [ span
                [ class "todo-count" ]
                [ strong [] [ text (toString tasksLeft) ]
                , text (item_ ++ " left")
                ]
            , ul
                [ class "filters" ]
                [ visibilitySwap "#/" All visibility
                , text " "
                , visibilitySwap "#/active" Active visibility
                , text " "
                , visibilitySwap "#/completed" Completed visibility
                ]
            , button
                [ class "clear-completed"
                , hidden (tasksCompleted == 0)
                , onClick DeleteComplete
                ]
                [ text ("Clear completed (" ++ toString tasksCompleted ++ ")") ]
            ]


visibilitySwap : String -> Visibility -> Visibility -> Html Msg
visibilitySwap uri visibility actualVisibility =
    let
        className =
            if visibility == actualVisibility then
                "selected"
            else
                ""
    in
        li
            [ onClick (ChangeVisibility visibility) ]
            [ a [ class className, href uri ] [ text (toString visibility) ] ]


infoFooter : Html msg
infoFooter =
    footer
        [ class "info" ]
        [ p [] [ text "Double-click to edit a todo" ]
        , p []
            [ text "Written by "
            , a [ href "https://github.com/evancz" ] [ text "Evan Czaplicki" ]
            ]
        , p []
            [ text "Part of "
            , a [ href "http://todomvc.com" ] [ text "TodoMVC" ]
            ]
        ]



-- wire the entire application together


main : Program Never Model Msg
main =
    Navigation.program SetVisibility
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }



-- URL PARSERS - check out evancz/url-parser for fancier URL parsing


urlParser : Parser (Visibility -> Visibility) Visibility
urlParser =
    oneOf
        [ UrlParser.map All top
        , UrlParser.map Active (UrlParser.s "active")
        , UrlParser.map Completed (UrlParser.s "completed")
        ]


setVisibility : Location -> Model -> Model
setVisibility location model =
    let
        visibility =
            parseHash urlParser location
                |> Maybe.withDefault All
    in
        { model | visibility = visibility }


init : Location -> ( Model, Cmd Msg )
init location =
    ( setVisibility location emptyModel, Cmd.none )



-- interactions with localStorage


port save : List Todo.Task.Model -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
