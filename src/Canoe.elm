port module Main exposing (Model, Msg(..), init, inputPort, main, outputPort, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (id, style, type_, attribute, placeholder, value, class, name, for)
import Html.Events exposing (onInput, onSubmit, onClick)

import Time
import Json.Encode
import Json.Decode



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
  , board : List (Int)
  , lastCell : Int
  , turn : Int
  , currentTimer : Int
  , debugString : String
  }

buildDefault : List (Int)
buildDefault = 
  [-1,  0,  0, -1, -1, -1, -1, -1, -1, -1,  0,  0, -1,
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  -1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, -1,
  -1, -1, -1,  0,  0,  0,  0,  0,  0,  0, -1, -1, -1]

init : () -> (Model, Cmd Msg)
init _ =
  (Model
    "board not really implemented"
    buildDefault
    3
    1
    0
    ""
  , Cmd.none )



-- UPDATE


type Msg
  = SetName String
  --| SetColor String
  --| UpdateSettings
  --| SetMessage String
  --| SendMessage
  | NewGame
  --| SetScore Int Int
  --| ToggleEmoticons
  --| InsertEmoticon String
  | Tick Time.Posix
  | Ping Time.Posix
  | GetJSON Json.Encode.Value              -- Parse incoming JSON
  | ConnectToServer Json.Encode.Value      -- 000
  --| GetBoard Json.Encode.Value             -- 100
  --| GetPegs Json.Encode.Value         -- 101
  --| GetScore Json.Encode.Value         -- 200
  --| GetChat Json.Encode.Value              -- 202
  --| SetActiveColor (Maybe Color)
  | AddMove Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case Debug.log "MESSAGE: " msg of
    SetName name ->
      ( { model | nameInProgress = name }, Cmd.none )
      
    NewGame -> -- TODO!
      ( { model | nameInProgress = "New game" }, Cmd.none )

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
      case (Json.Decode.decodeValue decodeJSON json) of
        Ok {action, content} ->
          case action of
            "update_chat" ->
              ((Debug.log "Error: unknown code in JSON message" model), Cmd.none ) -- Error: missing code
            _ ->
              ((Debug.log "Error: unknown code in JSON message" model), Cmd.none ) -- Error: missing code

        Err _ ->
          ( { model | debugString = ("Bad JSON: " ++ (Json.Encode.encode 0 json))}, Cmd.none )

    ConnectToServer json ->
      ( model,
        outputPort
          ( Json.Encode.encode
            0
            ( Json.Encode.object
              [ ("action", Json.Encode.string "create_user")
              , ("content", Json.Encode.string "") ] ))
        )


    -- Add move.
    AddMove n ->
      let
        newTurn = -1*model.turn+3
        newBoard = updateBoard 0 n newTurn model.board
      in
        ( { model | debugString = String.fromInt n, turn = newTurn, board = newBoard}, 
          outputPort
            ( Json.Encode.encode
              0
              ( Json.Encode.object
                [ ("action", Json.Encode.string "submit_movelist")
                , ("content", Json.Encode.string "todo") ] ))
        )
    

updateBoard : Int -> Int -> Int -> List (Int) -> List (Int)
updateBoard current target value board =
  case board of
    x::xs ->
      if current == target then
        value :: xs
      else
        x :: updateBoard (current+1) target value xs
    _ ->
      []

decodeJSON : Json.Decode.Decoder JSONMessage
decodeJSON =
  Json.Decode.map2
    JSONMessage
    (Json.Decode.field "action" Json.Decode.string)
    (Json.Decode.field "content" Json.Decode.value)


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

{- 
        <div class="c"></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"></div><div class="c"></div><div class="c"></div><div class="c"></div><div class="c"></div><div class="c"></div><div class="c"></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"></div>
        <div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s blue"></div></div><div class="c"><div class="s red"></div></div><div class="c"><div class="s red"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div>
        <div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s red"></div></div><div class="c"><div class="s blue"></div></div><div class="c"><div class="s blue"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div>
        <div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s blue"></div></div><div class="c"><div class="s red last"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s blue"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div>
        <div class="c"></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s red"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"></div>
        <div class="c"></div><div class="c"></div><div class="c"></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"></div><div class="c"></div><div class="c"></div>
-}

-- VIEW
drawCells : Int -> List (Int) -> List (Html Msg)
drawCells index remainingCells =
  case remainingCells of
    [] ->
      []
    v::vs ->
      case v of
        0 ->
          div [class "c"]
          [ div [ class "s", onClick (AddMove index) ] [] ] :: drawCells (index+1) vs
        1 ->
          div [class "c"]
          [ div [ class "s red" ] [] ] :: drawCells (index+1) vs
        2 ->
          div [class "c"]
          [ div [ class "s blue" ] [] ] :: drawCells (index+1) vs
        _ ->
          div [class "c"] [] :: drawCells (index+1) vs


view : Model -> Html Msg
view model =
  let
    drawBoard board = drawCells 0 board

  in 
    div [ class "container"]
    [ main_ []
      [ div [class "grid"]
        ( model.board |> drawBoard )
      , div [ class "requests" ]
        [ div [ class "player-colors" ]
          [ div [ class "player-colors__row" ]
            [ div [ class "s red" ] []
            , span []
              [ text "You"
              , em [] [ text "0" ]
              ]
            ]
          , div [ class "player-colors__row" ]
            [ div [ class "s blue" ] []
            , span []
              [ text "Opponent"
              , em [] [ text "0" ]
              ]
            ]
          ]
        , div [class "a"]
          [ text "Resign" ]
        , div [class "a"]
          [ text "Help" ]
        ]
      ]
    , div [] [ text model.debugString ]
    ]