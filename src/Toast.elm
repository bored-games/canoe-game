module Toast exposing
    ( Toast(..), defaultConfig, defaultView, Stack, Msg
    , config, delay, transitionOutDuration, containerAttrs, itemAttrs, transitionInAttrs, transitionOutAttrs, Config
    , view, update, addToast, addPersistentToast, addToastIf, addToastIfUnique, hasToast, initialState
    )

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html.Keyed
import Process
import Random exposing (Seed)
import Task

type Toast
    = Success String String
    | Warning String String
    | Error String String


type Stack a
    = Stack (List ( Id, Status, a )) Seed


{-| How the toast will be removed.

Temporary toasts are removed after a timeout or after a click,
Persistent toasts must be clicked to be removed.

-}
type RemoveBehaviour
    = Temporary
    | Persistent


{-| The internal message type used by the library. You need to tag and add it to your app messages.

    type Msg
        = ToastyMsg (Toasty.Msg MyToast)

-}
type Msg a
    = Add a
    | Remove Id
    | TransitionOut Id


{-| The base configuration type.
-}
type Config msg
    = Config
        { transitionOutDuration : Float
        , transitionOutAttrs : List (Html.Attribute msg)
        , transitionInAttrs : List (Html.Attribute msg)
        , containerAttrs : List (Html.Attribute msg)
        , itemAttrs : List (Html.Attribute msg)
        , delay : Float
        }


type alias Id =
    Int


type Status
    = Entered
    | Leaving


{-| Some basic configuration defaults: Toasts are visible for 5 seconds with
no animations or special styling.
-}
config : Config msg
config =
    Config
        { transitionOutDuration = 0
        , transitionOutAttrs = []
        , transitionInAttrs = []
        , containerAttrs = []
        , itemAttrs = []
        , delay = 3000
        }


{-| Changes the amount of time (in milliseconds) to wait after transition out
begins and before actually removing the toast node from the DOM. This lets you
author fancy animations when a toast is removed.
-}
transitionOutDuration : Float -> Config msg -> Config msg
transitionOutDuration time (Config cfg) =
    Config { cfg | transitionOutDuration = time }


{-| Lets you set the HTML attributes to add to the toast container when transitioning in.
-}
transitionInAttrs : List (Html.Attribute msg) -> Config msg -> Config msg
transitionInAttrs attrs (Config cfg) =
    Config { cfg | transitionInAttrs = attrs }


{-| Lets you set the HTML attributes to add to the toast container when transitioning out.
-}
transitionOutAttrs : List (Html.Attribute msg) -> Config msg -> Config msg
transitionOutAttrs attrs (Config cfg) =
    Config { cfg | transitionOutAttrs = attrs }


{-| Lets you set the HTML attributes to add to the toasts stack container. This will help
you style and position the toast stack however you like by adding classes or inline styles.
-}
containerAttrs : List (Html.Attribute msg) -> Config msg -> Config msg
containerAttrs attrs (Config cfg) =
    Config { cfg | containerAttrs = attrs }


{-| Lets you set the HTML attributes to add to each toast container. This will help
you style and arrange the toasts however you like by adding classes or inline styles.
-}
itemAttrs : List (Html.Attribute msg) -> Config msg -> Config msg
itemAttrs attrs (Config cfg) =
    Config { cfg | itemAttrs = attrs }


{-| Changes the amount of time (in milliseconds) the toast will be visible.
After this time, the transition out begins.
-}
delay : Float -> Config msg -> Config msg
delay time (Config cfg) =
    Config { cfg | delay = time }


{-| An empty stack of toasts to initialize your model with.
-}
initialState : Stack a
initialState =
    Stack [] (Random.initialSeed 0)


{-| Handles the internal messages. You need to wire it to your app update function

    update msg model =
        case msg of
            ToastyMsg subMsg ->
                Toasty.update Toasty.config ToastyMsg subMsg model

-}
update : Config msg -> (Msg a -> msg) -> Msg a -> { m | toastMessages : Stack a } -> ( { m | toastMessages : Stack a }, Cmd msg )
update (Config cfg) tagger msg model =
    let
        (Stack toasts seed) =
            model.toastMessages
    in
    case msg of
        Add toast ->
            addToast config tagger toast ( model, Cmd.none )

        Remove targetId ->
            let
                newStack =
                    List.filter (\( id, toast, status ) -> id /= targetId) toasts
            in
            ( { model | toastMessages = Stack newStack seed }
            , Cmd.none
            )

        TransitionOut targetId ->
            let
                newStack =
                    List.map
                        (\( id, status, toast ) ->
                            if id == targetId then
                                ( id, Leaving, toast )

                            else
                                ( id, status, toast )
                        )
                        toasts
            in
            ( { model | toastMessages = Stack newStack seed }
            , Task.perform (\_ -> tagger (Remove targetId)) (Process.sleep <| cfg.transitionOutDuration)
            )


{-| Adds a toast to the stack and schedules its removal. It receives and returns
a tuple of type '(model, Cmd msg)' so that you can easily pipe it to your app
update function branches.

    update msg model =
        case msg of
            SomeAppMsg ->
                ( newModel, Cmd.none )
                    |> Toasty.addToast myConfig ToastyMsg (MyToast "Entity successfully created!")

            ToastyMsg subMsg ->
                Toasty.update myConfig ToastyMsg subMsg model

-}
addToast : Config msg -> (Msg a -> msg) -> a -> ( { m | toastMessages : Stack a }, Cmd msg ) -> ( { m | toastMessages : Stack a }, Cmd msg )
addToast =
    addToast_ Temporary


{-| Similar to `addToast` but doesn't schedule the toast removal, so it will remain visible until clicked.
-}
addPersistentToast : Config msg -> (Msg a -> msg) -> a -> ( { m | toastMessages : Stack a }, Cmd msg ) -> ( { m | toastMessages : Stack a }, Cmd msg )
addPersistentToast =
    addToast_ Persistent


{-| Similar to `addToast` but also receives a condition parameter `List toast -> Bool`
so that the toast will only be added if the condition returns `True`.
-}
addToastIf : Config msg -> (Msg a -> msg) -> (List a -> Bool) -> a -> ( { m | toastMessages : Stack a }, Cmd msg ) -> ( { m | toastMessages : Stack a }, Cmd msg )
addToastIf cfg tagger condition toast ( model, cmd ) =
    let
        (Stack toasts seed) =
            model.toastMessages

        shouldAddToast =
            toasts
                |> List.map (\( id, st, t ) -> t)
                |> condition
    in
    if shouldAddToast then
        addToast cfg tagger toast ( model, cmd )

    else
        ( model
        , Cmd.none
        )


{-| Similar to `addToast` but only effectively adds the toast if it's not already
present in the stack. This is a convenience `addToastIf` function using
`not << List.member toast` as a `condition` parameter.
-}
addToastIfUnique : Config msg -> (Msg a -> msg) -> a -> ( { m | toastMessages : Stack a }, Cmd msg ) -> ( { m | toastMessages : Stack a }, Cmd msg )
addToastIfUnique cfg tagger toast ( model, cmd ) =
    addToastIf cfg tagger (not << List.member toast) toast ( model, cmd )


{-| Figure out whether a stack contains a specific toast. Similar to `List.member`.
-}
hasToast : a -> Stack a -> Bool
hasToast toast (Stack toasts _) =
    toasts
        |> List.map (\( _, _, t ) -> t)
        |> List.member toast


addToast_ : RemoveBehaviour -> Config msg -> (Msg a -> msg) -> a -> ( { m | toastMessages : Stack a }, Cmd msg ) -> ( { m | toastMessages : Stack a }, Cmd msg )
addToast_ removeBehaviour (Config cfg) tagger toast ( model, cmd ) =
    let
        (Stack toasts seed) =
            model.toastMessages

        ( newId, newSeed ) =
            getNewId seed

        task =
            case removeBehaviour of
                Temporary ->
                    Task.perform (\() -> tagger (TransitionOut newId)) (Process.sleep <| cfg.delay)

                Persistent ->
                    Cmd.none
    in
    ( { model | toastMessages = Stack (toasts ++ [ ( newId, Entered, toast ) ]) newSeed }
    , Cmd.batch [ cmd, task ]
    )


{-| Renders the stack of toasts. You need to add it to your app view function and
give it a function that knows how to render your toasts model.

    view model =
        div []
            [ h1 [] [ text "Toasty example" ]
            , Toasty.view myConfig (\txt -> div [] [ text txt ]) ToastyMsg model.toastMessages
            ]

-}
view : Config msg -> (a -> Html msg) -> (Msg a -> msg) -> Stack a -> Html msg
view cfg toastView tagger (Stack toasts seed) =
    let
        (Config c) =
            cfg
    in
    if List.isEmpty toasts then
        text ""

    else
        Html.Keyed.ol c.containerAttrs <| List.map (\toast -> itemContainer cfg tagger toast toastView) toasts


getNewId : Seed -> ( Id, Seed )
getNewId seed =
    Random.step (Random.int Random.minInt Random.maxInt) seed


itemContainer : Config msg -> (Msg a -> msg) -> ( Id, Status, a ) -> (a -> Html msg) -> ( String, Html msg )
itemContainer (Config cfg) tagger ( id, status, toast ) toastView =
    let
        attrs =
            case status of
                Entered ->
                    cfg.transitionInAttrs

                Leaving ->
                    cfg.transitionOutAttrs
    in
    ( String.fromInt id, li (cfg.itemAttrs ++ attrs ++ [ onClick (tagger <| TransitionOut id) ]) [ toastView toast ] )

-- DEFAULTS:
defaultConfig : Config msg
defaultConfig =
    config
    |> transitionOutDuration 700
    |> transitionOutAttrs defaultTransitionOutAttrs
    |> transitionInAttrs defaultTransitionInAttrs
    |> containerAttrs defaultContainerAttrs
    |> itemAttrs defaultItemAttrs
    |> delay 5000

    
defaultContainerAttrs : List (Html.Attribute msg)
defaultContainerAttrs =
    [ style "position" "fixed"
    , style "top" "0"
    , style "right" "0"
    , style "width" "100%"
    , style "max-width" "300px"
    , style "list-style-type" "none"
    , style "padding" "0"
    , style "margin" "0"
    ]


defaultItemAttrs : List (Html.Attribute msg)
defaultItemAttrs =
    [ style "margin" "1em 1em 0 1em"
    , style "max-height" "100px"
    , style "transition" "max-height 0.6s, margin-top 0.6s"
    ]


defaultTransitionInAttrs : List (Html.Attribute msg)
defaultTransitionInAttrs =
    [ class "animated bounceInRight"
    ]


defaultTransitionOutAttrs : List (Html.Attribute msg)
defaultTransitionOutAttrs =
    [ class "animated fadeOutRightBig"
    , style "max-height" "0"
    , style "margin-top" "0"
    ]


{-| Default theme view handling the three toast variants.
-}
defaultView : Toast -> Html msg
defaultView toast =
    case toast of
        Success title message ->
            genericToast "toasty-success" title message

        Warning title message ->
            genericToast "toasty-warning" title message

        Error title message ->
            genericToast "toasty-error" title message


genericToast : String -> String -> String -> Html msg
genericToast variantClass title message =
    div
        [ class "toasty-container", class variantClass ]
        [ h1 [ class "toasty-title" ] [ text title ]
        , if String.isEmpty message then
            text ""

          else
            p [ class "toasty-message" ] [ text message ]
        ]