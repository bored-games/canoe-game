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
  , board : List ( List (Int) )
  , lastCell : Int
  , turn : Int
  , currentTimer : Int
  , debugString : String
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
  | AddMove Int Int


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
      case Json.Decode.decodeValue decodeJSON json of
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
    AddMove tx ty ->
      let
        newTurn = -1*model.turn+3
        newBoard = updateRows 0 tx ty newTurn model.board
      in
        ( { model | debugString = String.fromInt tx ++ ", " ++ String.fromInt ty, turn = newTurn, board = newBoard}, 
          outputPort
            ( Json.Encode.encode
              0
              ( Json.Encode.object
                [ ("action", Json.Encode.string "submit_movelist")
                , ("content", Json.Encode.string "todo") ] ))
        )
    

updateRows : Int -> Int -> Int -> Int -> List ( List (Int )) -> List ( List (Int) )
updateRows iy tx ty value board =
  case board of
    r::rs ->
      updateCol 0 iy tx ty value r::updateRows (iy+1) tx ty value rs
    _ ->
      []

      
updateCol : Int -> Int -> Int -> Int -> Int -> List (Int ) -> List (Int)
updateCol ix iy tx ty value board =
  case board of
    c::cs ->
      if (ix, iy) == (tx, ty) then
        value :: cs
      else
        c :: updateCol (ix+1) iy tx ty value cs
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
drawCells : Int -> Int -> List (Int) -> List (Html Msg)
drawCells x y remainingCells =
  case remainingCells of
    [] ->
      []
    v::vs ->
      case v of
        0 ->
          div [class "c"]
          [ div [ class "s", onClick (AddMove x y) ] [] ] :: drawCells (x+1) y vs
        1 ->
          div [class "c"]
          [ div [ class "s red" ] [] ] :: drawCells (x+1) y vs
        2 ->
          div [class "c"]
          [ div [ class "s blue" ] [] ] :: drawCells (x+1) y vs
        _ ->
          div [class "c"] [] :: drawCells (x+1) y vs

drawRows : Int -> List ( List (Int)) -> List (Html Msg)
drawRows y remainingRows =
  case remainingRows of
    [] ->
      []
    r::rs ->
      List.append (drawCells 0 y r) (drawRows (y+1) rs)

view : Model -> Html Msg
view model =
  let
    drawBoard board = drawRows 0 board

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