port module Canoe exposing (Model, Msg(..), init, inputPort, main, outputPort, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (id, style, type_, attribute, placeholder, value, class, name, for)
import Html.Events exposing (onInput, onSubmit, onClick)

import Toast


import Dict
import Set exposing (Set)
import Tuple
import Time
import Json.Encode
import Json.Decode

import User exposing (User)



-- MAIN


main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias JSONMessage = 
  { action : String 
  , content : Json.Encode.Value
  }

type alias Model =
  { nameInProgress : String
  , topMessage : String
  , toastMessages :  Toast.Stack Toast.Toast
  , board : List ( List (Int) )
  , lastMove : (Int, Int)
  , selectedReds : Set (Int, Int)
  , selectedBlues : Set (Int, Int)
  , lastCell : Int
  , turn : Int
  , currentTimer : Int
  , debugString : String
  , red : Maybe User
  , blue : Maybe User
  , user : Maybe User
  , users : List ( User )
  , gameOver : Bool
  , showHelp : Bool
  }

buildDefault : List ( List (Int) )
buildDefault = 
  [[-1,  0,  0, -1, -1, -1, -1, -1, -1, -1,  0,  0, -1],
  [0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0],
  [0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0],
  [0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0],
  [-1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, -1],
  [-1, -1, -1,  0,  0,  0,  0,  0,  0,  0, -1, -1, -1]]

init : () -> (Model, Cmd Msg)
init _ =
  (Model
    ""
    ""
    Toast.initialState
    buildDefault
    (4, 4)
    Set.empty
    Set.empty
    3
    1
    0
    " "
    Nothing
    Nothing
    Nothing
    []
    False
    False
  , Cmd.none )




-- UPDATE


type Msg
  = SetName String
  | NewGame
  | Tick Time.Posix
  | Ping Time.Posix
  | GetJSON Json.Encode.Value              -- Parse incoming JSON
  | GetBoard Json.Encode.Value
  | GetUsersList Json.Encode.Value
  | GetUser Json.Encode.Value
  | GetMessage Json.Encode.Value
  | GetFlashMessage Json.Encode.Value
  | GetLastMove Json.Encode.Value
  | GameOver Json.Encode.Value
  | SendNewGame
  | SendResign
  | ConnectToServer Json.Encode.Value
  | SetTeam Int
  | AddMove Int Int
  | AddToastMessage (Toast.Msg Toast.Toast)
  | TestToast String
  | ToggleHelp


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of -- case Debug.log "MESSAGE: " msg of
    SetName name ->
      ( { model | nameInProgress = name }, Cmd.none )
      
    NewGame -> -- TODO!
      ( { model | gameOver = False }, Cmd.none )
      

    Tick newTime ->
      let
          currentTimerDisplay = model.currentTimer + 1
      in
          ( { model | currentTimer = currentTimerDisplay }, Cmd.none )

    Ping newTime ->
      ( { model | currentTimer = (model.currentTimer + 1) }
        , outputPort (Json.Encode.encode
                        0
                      ( Json.Encode.object
                      [ ( "action", Json.Encode.string "ping"),
                        ( "content", Json.Encode.string "ping" ) ] ) )
      )

    GetJSON json ->
      case Json.Decode.decodeValue decodeJSON json of
        Ok {action, content} ->
          case action of
            "update_chat" ->
              ((Debug.log "Error: not implemented" model), Cmd.none ) -- Error: missing code
            "update_scoreboard" ->
              update (GetUsersList content) model
            "update_user" ->
              update (GetUser content) model
            "update_board" ->
              update (GetBoard content) model
            "update_message" ->
              update (GetMessage content) model
            "update_flash_msg" ->
              update (GetFlashMessage content) model
            "update_last_move" ->
              update (GetLastMove content) model
            "game_over" ->
              update (GameOver content) model
            _ ->
              ((Debug.log "Error: unknown code in JSON message" model), Cmd.none ) -- Error: missing code

        Err _ ->
          ( { model | debugString = ("Bad JSON: " ++ (Json.Encode.encode 0 json))}, Cmd.none )

    GetBoard json ->
      case Json.Decode.decodeValue (Json.Decode.list (Json.Decode.list Json.Decode.int)) json of
        Ok board ->
          ( { model | board = board}, Cmd.none )
        Err _ ->
          ( { model | debugString = "Critical error getting new board"}, Cmd.none )

    GetUsersList json ->
      case Json.Decode.decodeValue User.decodeUsersList json of
        Ok usersList ->
          let
            red_user =
              case List.filter (\z -> .team z == 1) (Dict.values usersList) of
                []     -> Nothing
                u::_ -> Just u
            blue_user =
              case List.filter (\z -> .team z == 2) (Dict.values usersList) of
                []     -> Nothing
                u::_ -> Just u
          in
            ( { model | users = Dict.values usersList, red = red_user, blue = blue_user }, Cmd.none )
        Err _ ->
          ( { model | debugString = "Error parsing userlist JSON"}, Cmd.none )

    GetUser json ->
      case Json.Decode.decodeValue User.decodeUser json of
        Ok user ->
          ( { model | user = Just user}, Cmd.none )
        Err _ ->
          ( { model | debugString = "Error parsing user JSON"}, Cmd.none )

    GetMessage json ->
      case Json.Decode.decodeValue Json.Decode.string json of
        Ok message ->
          ( { model | topMessage = message}, Cmd.none )
        Err _ ->
          ( { model | debugString = "Error parsing msg JSON"}, Cmd.none )

    GetFlashMessage json ->
      case Json.Decode.decodeValue Json.Decode.string json of
        Ok message ->
          ( { model | debugString = message}, Cmd.none )
            |> addToast (Toast.Success "" message)
        Err _ ->
          ( { model | debugString = "Error parsing Flash Message JSON"}, Cmd.none )

    GetLastMove json ->
      case Json.Decode.decodeValue decodeMoveTuple json of
        Ok tuple ->
          ( { model | lastMove = tuple}, Cmd.none )
        Err _ ->
          ( { model | debugString = "Error parsing Flash Message JSON"}, Cmd.none )

    GameOver json ->
      case Json.Decode.decodeValue Json.Decode.string json of
        Ok message ->
          ( { model | gameOver = True, topMessage = message }, Cmd.none )
        Err _ ->
          ( { model | debugString = "Error parsing user JSON"}, Cmd.none )

    ConnectToServer _ ->
      ( model,
        outputPort
          ( Json.Encode.encode
            0
            ( Json.Encode.object
              [ ("action", Json.Encode.string "create_user")
              , ("content", Json.Encode.string "") ] ))
        )

    SendNewGame ->
      ( model,
        outputPort
          ( Json.Encode.encode
              0
              ( Json.Encode.object
                [ ("action", Json.Encode.string "game_action")
                , ("content", Json.Encode.object
                  [ ("action", Json.Encode.string "new_game"),
                    ("content", Json.Encode.string "") ] ) ] ) ) )

    SendResign ->
      ( model,
        outputPort
          ( Json.Encode.encode
              0
              ( Json.Encode.object
                [ ("action", Json.Encode.string "game_action")
                , ("content", Json.Encode.object
                  [ ("action", Json.Encode.string "resign"),
                    ("content", Json.Encode.string "") ] ) ] ) ) )

    SetTeam team ->
      ( model, outputPort
            ( Json.Encode.encode
              0
              ( Json.Encode.object
                [ ("action", Json.Encode.string "game_action")
                , ("content", Json.Encode.object
                  [ ("action", Json.Encode.string "set_team"),
                    ("content", Json.Encode.int team) ] ) ] ) ) )

    -- Add move.
    AddMove tx ty ->
      let
        newTurn = -1*model.turn+3
      in
        ( { model | turn = newTurn}, 
          outputPort
            ( Json.Encode.encode
              0
              ( Json.Encode.object
                [ ("action", Json.Encode.string "game_action")
                , ("content", Json.Encode.object
                  [ ("action", Json.Encode.string "submit_movelist"),
                    ("content", Json.Encode.list Json.Encode.int [tx, ty])
                  ]
                )
              ]
            )
          )
        )

    AddToastMessage subMsg ->
        Toast.update toastConfig AddToastMessage subMsg model

    TestToast str ->
        ( model, Cmd.none )
            |> addToast (Toast.Success "Allright!" "Thing successfully updated")

    ToggleHelp ->
        ( { model | showHelp = not model.showHelp }, Cmd.none )



toastConfig : Toast.Config Msg
toastConfig =
    Toast.defaultConfig |> Toast.delay 3300


-- addToast : Toast.Toast -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
-- addToast toast ( model, cmd ) =
--     Toast.addToast toastConfig AddToastMessage toast ( model, cmd )

    
addToast : Toast.Toast -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addToast toast ( model, cmd ) =
    Toast.addToast toastConfig AddToastMessage toast ( model, cmd )

decodeJSON : Json.Decode.Decoder JSONMessage
decodeJSON =
  Json.Decode.map2
    JSONMessage
    (Json.Decode.field "action" Json.Decode.string)
    (Json.Decode.field "content" Json.Decode.value)
    
decodeMoveTuple : Json.Decode.Decoder (Int, Int)
decodeMoveTuple =
  Json.Decode.map2 Tuple.pair
    (Json.Decode.index 0 Json.Decode.int)
    (Json.Decode.index 1 Json.Decode.int)

decodeTeams : Json.Decode.Decoder ( String, String )
decodeTeams =
  Json.Decode.map2
    Tuple.pair
    (Json.Decode.field "red" Json.Decode.string)
    (Json.Decode.field "blue" Json.Decode.string)


-- SUBSCRIPTIONS

port outputPort : (String) -> Cmd msg
port inputPort : (Json.Encode.Value -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ Time.every 50000 Ping
    , Time.every 1000 Tick
    , inputPort GetJSON
    ]


-- VIEW
drawCells : Int -> Int -> List (Int) -> (Int, Int) -> List (Html Msg)
drawCells x y remainingCells lastMove =
  case remainingCells of
    [] ->
      []
    v::vs ->
      case v of
        0 ->
          div [class "c"]
          [ div [ class "s", onClick (AddMove x y) ] [] ] :: drawCells (x+1) y vs lastMove
        1 ->
          div [class "c"]
          [ div [ class ("s red" ++ if (x, y) == lastMove then " pulse" else "") ] [] ] :: drawCells (x+1) y vs lastMove
        2 ->
          div [class "c"]
          [ div [ class ("s blue" ++ if (x, y) == lastMove then " pulse" else "") ] [] ] :: drawCells (x+1) y vs lastMove
        _ ->
          div [class "c"] [] :: drawCells (x+1) y vs lastMove

drawRows : Int -> List ( List (Int)) -> (Int, Int) -> List (Html Msg)
drawRows y remainingRows lastMove =
  case remainingRows of
    [] ->
      []
    r::rs ->
      List.append (drawCells 0 y r lastMove) (drawRows (y+1) rs lastMove)

isSameUser : Maybe User -> Maybe User -> Bool
isSameUser u1 u2 =
  case u1 of
     Nothing -> False
     Just a ->
      case u2 of
        Nothing -> False
        Just b -> a.username == b.username

formatName : Maybe User -> String -> Bool -> List ( Html Msg )
formatName user color isUser =
  case user of
     Nothing ->
      [ div [ class ("s " ++ color) ] []
      , span []
        [ text "Waiting..."
        , em [] [ text "0" ]
        ]
      ]
     Just u  ->
      [ div [ class ("s " ++ color) ] []
        , span [ class (if isUser then "bold" else "") ]
          [ text u.nickname
          , em [] [ text (String.fromInt u.score) ]
          ]
        ]

modalSpectators : Maybe User -> Maybe User -> List ( User )-> String
modalSpectators red_user blue_user all_users =
  String.join ", " (List.map .nickname all_users)

modalUser : Maybe User -> String -> Int -> Html Msg
modalUser user color teamid =
  case user of
     Nothing -> div [ class ("modal_" ++ String.toLower color), onClick (SetTeam teamid) ] [ div [ class "pad" ] [ h3 [] [ text (color ++ " player") ], h4 [] [ text "Click to join" ] ] ]
     Just u -> div [ class ("inactive modal_" ++ String.toLower color), onClick (SetTeam 0) ] [ div [ class "pad" ] [ h3 [] [ text (color ++ " player") ], h4 [] [ text u.nickname ] ] ]


showModal : Maybe User -> Maybe User -> List (User) -> Html Msg
showModal red blue users = 
  div [ class "lightbox" ]
  [ div [ class "modal"]
    [ div [ class "flex_container" ]
      [ modalUser red "Red" 1
      , modalUser blue "Blue" 2
      , div [ class "modal_spectators" ] [ h3 [] [ text "Spectators" ], text (modalSpectators red blue users) ]
      ]
    ]
  ]
  
showHelp : Html Msg
showHelp = 
  div [ class "lightbox" ]
  [ div [ class "modal"]
    [ div [ class "flex_container" ] [ text "HELP YA" ]
    , div [] [ button [ class "close", onClick ToggleHelp ] [ text "Close" ] ]
    ]
  ]


view : Model -> Html Msg
view model =
  let
    drawBoard board lastMove = drawRows 0 board lastMove
  in 
    div [ class "container"]
    [ main_ []
      [ div [ class "top" ]
        [ div [ class "top_message" ] [ text model.topMessage ]
        -- , div [ class (if model.debugString == "" then "" else "flash_message")] [ text model.debugString ]
        , div [ class "toast_container"] [ Toast.view toastConfig Toast.defaultView AddToastMessage model.toastMessages ]
        ]
      , div [ class "grid" ]
        ( drawBoard model.board model.lastMove )
      , div [ class "requests" ]
        [ div [ class "player-colors" ]
          [ div [ class "player-colors__row" ]
            (formatName model.red "red" (isSameUser model.user model.red))
          , div [ class "player-colors__row" ]
            (formatName model.blue "blue" (isSameUser model.user model.blue))
          ]
        , div [class "game_buttons"]
          [ if model.gameOver then text "" else button [ onClick SendResign ] [ text "Resign" ]
          , button [ onClick SendNewGame ] [ text "New Game" ] ]
        , div [class "help_buttons"]
          [ button [ id "help", onClick ToggleHelp ] [ text "Help" ] ]
        ]
      ]
    , if model.red == Nothing || model.blue == Nothing then showModal model.red model.blue model.users else div [] []
    , if model.showHelp then showHelp else div [] []
    ]