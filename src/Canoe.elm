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

import Chat exposing (Chatline)
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
  { room_name : String
  , nameInProgress : String
  , colorInProgress : String
  , messageInProgress : String
  , chat : List Chatline
  , toggleStates :
    { settings: String
    , pollOptions: String
    , emoticons: String
    }
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
    ""
    ""
    [ ]
    { settings = "none",
      pollOptions = "none",
      emoticons = "none" }   
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
  | SetColor String
  | UpdateSettings
  | SetMessage String
  | SendMessage
  | TogglePollOptions
  | ToggleSettings
  | ToggleEmoticons
  | InsertEmoticon String
  | Tick Time.Posix
  | Ping Time.Posix
  | GetJSON Json.Encode.Value              -- Parse incoming JSON
  | GetBoard Json.Encode.Value
  | GetUsersList Json.Encode.Value
  | GetUser Json.Encode.Value
  | GetMessage Json.Encode.Value
  | GetFlashMessage Json.Encode.Value
  | GetLastMove Json.Encode.Value
  | NewGame Json.Encode.Value
  | GameOver Json.Encode.Value
  | GetChat Json.Encode.Value
  | ConnectToServer Json.Encode.Value
  | SendNewGame
  | SendResign
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
      
    SetColor color ->
      ( { model | colorInProgress = color }, Cmd.none )
      
    UpdateSettings ->
      let
        oldUser =
          case model.user of
            Nothing -> User "" "" "" 0 0 False False
            Just u -> u

        oldUsers = model.users
        oldName = oldUser.nickname
        oldToggleStates = model.toggleStates
        newColor = model.colorInProgress
        newName = if List.member model.nameInProgress (List.map .nickname oldUsers) then
                    oldName
                  else
                    if String.length model.nameInProgress > 0 then model.nameInProgress else oldName
        newUser = { oldUser | nickname = newName, color = newColor }
        replaceUser testUser =
          if oldUser.nickname == testUser.nickname then
            { testUser | nickname = newName, color = newColor }
          else
            testUser
        newUsers = List.map replaceUser oldUsers
        newToggleStates = { oldToggleStates | settings = "none" }
      in
      ( { model
       | user = Just newUser
       , users = newUsers
       , nameInProgress = ""
       , toggleStates = newToggleStates
      }
      , outputPort (Json.Encode.encode 0 (Json.Encode.object [ ("action", Json.Encode.string "update_user"), ("content", User.encodeUser newUser) ] ) ) )

    SetMessage message ->
      ( { model | messageInProgress = message }
      , Cmd.none
      )
      
    SendMessage ->
      let
        newmsg = String.trim model.messageInProgress
      in
        if newmsg == "" then
          ( { model | messageInProgress = "" }, Cmd.none )
        else
          ( { model | messageInProgress = "" }
          , outputPort (Json.Encode.encode
                          0
                        ( Json.Encode.object
                        [ ( "action", Json.Encode.string "update_chat"),
                          ( "content", Chat.encodeChatline model.room_name (Maybe.withDefault (User "" "" "" 0 0 False False) model.user) newmsg 0 ) ] ) ) )

    TogglePollOptions ->
      let
        oldToggleStates = model.toggleStates
        newToggleStates =
          { oldToggleStates
          | pollOptions = if oldToggleStates.pollOptions == "none" then "flex" else "none"
          , settings = "none"
          , emoticons = "none" }
      in
        ( { model | toggleStates = newToggleStates }
        , Cmd.none
        )
      

    ToggleSettings ->
      let
        oldToggleStates = model.toggleStates
        newToggleStates =
          { oldToggleStates
            | settings = if oldToggleStates.settings == "none" then "flex" else "none"
            , pollOptions = "none"
            , emoticons = "none" }
      in
        ( { model | toggleStates = newToggleStates }, Cmd.none )
      
    ToggleEmoticons ->
      let
        oldToggleStates = model.toggleStates
        newToggleStates =
          { oldToggleStates
          | emoticons = if oldToggleStates.emoticons == "none" then "flex" else "none"
          , pollOptions = "none"
          , settings = "none" }
      in
        ( { model | toggleStates = newToggleStates }, Cmd.none )
    
    InsertEmoticon str ->
      ( { model | messageInProgress = model.messageInProgress ++ " :" ++ str ++ ": " }, Cmd.none )


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
            "new_game" ->
              update (NewGame content) model
            "update_chat" ->
              update (GetChat content) model
            "player_chat_new_message" ->
              update (GetChat content) model
            "system_chat_new_message" ->
              update (GetChat content) model
            "system_chat_to_player_new_message" ->
              update (GetChat content) model
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
          
    NewGame json ->
      case Json.Decode.decodeValue Json.Decode.string json of
        Ok message ->
          ( { model | gameOver = False, topMessage = message }, Cmd.none )
        Err _ ->
          ( { model | debugString = "Error parsing game_over JSON"}, Cmd.none )

    GameOver json ->
      case Json.Decode.decodeValue Json.Decode.string json of
        Ok message ->
          ( { model | gameOver = True, topMessage = message }, Cmd.none )
        Err _ ->
          ( { model | debugString = "Error parsing game_over JSON"}, Cmd.none )

    GetChat json ->
      case Json.Decode.decodeValue Chat.decodeChatline json of
        Ok chatline ->
          ( { model | chat = chatline::model.chat}, Cmd.none )
        Err _ ->
          ( { model | debugString = "Error parsing chat JSON"}, Cmd.none )

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
    [ Time.every 30000 Ping
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

      
drawEmoticon : String -> Html Msg
drawEmoticon str =
  div [ class ("emoticon emoticon--" ++ str), onClick (InsertEmoticon str) ] []

emoticonList : List (String)
emoticonList = [ "cool", "crazy", "damn", "geek", "grin", "huh", "lol", "love", "omg", "pout", "sad", "smile", "stars", "ugh", "waiting", "whoopsy", "wink", "wtf" ]

drawEmoticons : List (Html Msg)
drawEmoticons =
  List.map drawEmoticon emoticonList

drawMessage : Chatline -> Html Msg
drawMessage message =
  case message.kind of
    2 -> (div [ class "chat__line" ] [ object [ class "chat__line--svg", attribute "data" message.message ] [ ] ])
    _ -> case message.user of
           Just user -> -- regular chat message
             ( div [ class "chat__line" ] 
               ( div [ class "chat__username", style "color" user.color ]
                 [ text user.nickname ] :: parseEmoticonHtml message.message )
             )
           _ -> -- system message
             (div [ class "chat__line" ] [ em [ class "chat__line--system" ] [ text message.message ] ])

drawSettings : Model -> List (Html Msg)
drawSettings model =
  [ h2 [ ] [ text "Settings" ]
  , div [ class "settings__flexbox" ]
  [ div [ class "setting__input" ] [ input [ type_ "text", onInput SetName, placeholder "New name", value model.nameInProgress ] [] ]
  , div [ class "setting__input" ]
    [ select [ onInput SetColor ]
      [ option [ value "", style "color" "#707070" ] [ text "Change color" ]
      , option [ value "#e05e5e", style "color" "#e05e5e" ] [ text "red" ]
      , option [ value "#e09f5e", style "color" "#e09f5e" ] [ text "orange" ]
      , option [ value "#e0e05e", style "color" "#e0e05e" ] [ text "yellow" ]
      , option [ value "#9fe05e", style "color" "#9fe05e" ] [ text "lime" ]
      , option [ value "#5ee05e", style "color" "#5ee05e" ] [ text "dark sea" ]
      , option [ value "#5ee09f", style "color" "#5ee09f" ] [ text "aquamarine" ]
      , option [ value "#5ee0e0", style "color" "#5ee0e0" ] [ text "azure" ]
      , option [ value "#5e9fe0", style "color" "#5e9fe0" ] [ text "cornflower" ]
      , option [ value "#5e5ee0", style "color" "#5e5ee0" ] [ text "periwinkle" ]
      , option [ value "#9f5ee0", style "color" "#9f5ee0" ] [ text "dendrobium " ]
      , option [ value "#e05ee0", style "color" "#e05ee0" ] [ text "french rose" ]
      , option [ value "#e05e9f", style "color" "#e05e9f" ] [ text "barbie-mobile" ]
      , option [ value "#b19278", style "color" "#b19278" ] [ text "english elm" ]
      , option [ value "#e0e0e0", style "color" "#e0e0e0" ] [ text "gainsboro" ]
      ]
    ]
  , div [ class "setting__submit" ] [ input [ type_ "submit", class "submit", value "Update", onClick UpdateSettings ] [] ]
    ]
  ]
             
drawPollOptions : List (Html Msg)
drawPollOptions =
  [ h2 [ ] [ text "Poll Commands" ]
  , div [ class "poll__info" ] [ text "Use /poll <command> or /set <command> to change settings. UIDs can be found by hovering over usernames in the scoreboard." ]
  , div [ class "poll__command", attribute "flow" "left", attribute "tooltip" "Give 'owner' status to user. Owners can use '/set'." ] [ text "owner ", span [ class "red" ] [ text "UID" ] ]
  , div [ class "poll__command", attribute "flow" "left", attribute "tooltip" "Remove 'owner' status from user." ] [ text "demote ", span [ class "red" ] [ text "UID" ] ]
  , div [ class "poll__command", attribute "flow" "left", attribute "tooltip" "Mute user. Muted users cannot chat or create polls." ] [ text "mute ", span [ class "red" ] [ text "UID" ] ]
  , div [ class "poll__command", attribute "flow" "left", attribute "tooltip" "Unmute user." ] [ text "unmute ", span [ class "red" ] [ text "UID" ] ]
  , div [ class "poll__command", attribute "flow" "left", attribute "tooltip" "Kick user from the game." ] [ text "kick ", span [ class "red" ] [ text "UID" ] ]
  , div [ class "poll__command", attribute "flow" "left", attribute "tooltip" "Set score of user to some number." ] [ text "score ", span [ class "red" ] [ text "UID " ], span [ class "blue" ] [ text "int" ] ]
  , div [ class "poll__command", attribute "flow" "left", attribute "tooltip" "Reset all scores to 0." ] [ text "reset_scores" ]
  , div [ class "poll__command", attribute "flow" "left", attribute "tooltip" "Reset board walls, goals, and robot positions." ] [ text "reset" ]
  , div [ class "poll__command", attribute "flow" "left", attribute "tooltip" "Reset goal position." ] [ text "new" ]
  , div [ class "poll__command", attribute "flow" "left", attribute "tooltip" "Set time limit for polls in seconds. Must be at least 30." ] [ text "poll_time ", span [ class "blue" ] [ text "int" ] ]
  , div [ class "poll__command", attribute "flow" "left", attribute "tooltip" "Set time limit for finding new solutions. Must be at least 0."] [ text "countdown_time ", span [ class "blue" ] [ text "int" ] ]
  , div [ class "poll__command", attribute "flow" "left", attribute "tooltip" "Set number of puzzles before a new board is shuffled." ] [ text "puzzles_before_new_board ", span [ class "blue" ] [ text "int" ] ]
  , div [ class "poll__command", attribute "flow" "left", attribute "tooltip" "1-robot solutions below this number will not add to score." ] [ text "min_moves ", span [ class "blue" ] [ text "int" ] ]
  ]

parseEmoticonHtml : String -> List (Html Msg)
parseEmoticonHtml str =
  let
    parseEmoticon ind1 ind2 teststr =
      let
        parsedStr = String.slice (ind1+1) ind2 teststr
      in
        if List.member parsedStr emoticonList then
          span [ class ("emoticon emoticon--" ++ parsedStr) ] [] :: parseEmoticonHtml (String.dropLeft (ind2+1) str)
        else
          text (":"++parsedStr) :: parseEmoticonHtml (String.dropLeft ind2 str)
  in
    case String.indexes ":" str of
      a::b::_ ->
        text (String.slice 0 a str)
        :: parseEmoticon a b str

      _ ->
        [ text str ]

onEnter : msg -> Attribute msg
onEnter msg =
  let
    filterKey code =
      if code == 13 then -- Enter was pressed
        Json.Decode.succeed { message = msg, stopPropagation = False, preventDefault = True }
      else
        Json.Decode.fail "ignored input"
    decoder =
      Html.Events.keyCode |> Json.Decode.andThen filterKey
  
  in 
    Html.Events.custom "keydown" decoder

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
        , span (if isUser then [class "bold", attribute "flow" "up", attribute "tooltip" "This is you!"] else [])
          [ text u.nickname
          , em [] [ text (String.fromInt u.score) ]
          ]
        ]

modalSpectators : Maybe User -> Maybe User -> List ( User )-> String
modalSpectators red_user blue_user all_users =
  let
    spectators = List.filter (\x -> not (isSameUser (Just x) red_user) && not (isSameUser (Just x) blue_user)) all_users
  in
    String.join ", " (List.map .nickname spectators)

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
    [ div [ class "flex_container" ]
      [ h2 [] [ text "Instructions" ]
      , p [] [ text "Players take turns placing \"tokens\" trying to complete four-token \"canoes\". A complete, valid canoe is as follows:" ]
      , div [ class "help--canoes" ] [ ]
      , p [] [ text "The first player to complete two separate canoes (sharing no tokens) wins!" ]
      ]
    , div [ class "margin-left" ] [ button [ class "close", onClick ToggleHelp ] [ text "Close" ] ]
    ]
  ]


view : Model -> Html Msg
view model =
  let
    drawChat chat =
      List.map drawMessage chat

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
    , aside [ class "sidebar", attribute "data-role" "sidebar" ]
      [ h2 [] [ text "Chat" ]
      , div [class "chat", id "chat"] (List.reverse model.chat |> drawChat)
      , div [ class ("sidebar__settings " ++ ("module-" ++ model.toggleStates.settings)) ] (drawSettings model)
      , div [ class ("sidebar__polloptions " ++ ("module-" ++ model.toggleStates.pollOptions)) ] drawPollOptions
      , div [ class "message"]
        [ textarea [ class "message__box", onEnter SendMessage, onInput SetMessage, placeholder "Send a message", value model.messageInProgress, Html.Attributes.maxlength 255 ] []
        , div [ class ("sidebar__emoticons " ++ ("module-" ++ model.toggleStates.emoticons)) ] drawEmoticons
        , div [ class "message__actions" ]
          [
          button [ class "settings", onClick ToggleSettings ] []
        , button [ class "poll", onClick TogglePollOptions ] []
        , div [ class "flex-spacer" ] []
        , button [ class "emoticons", onClick ToggleEmoticons ] []
        , input [ type_ "submit", class "submit", value "Send", onClick SendMessage ] []
          ]
        ]
      ]
    , if model.red == Nothing || model.blue == Nothing then showModal model.red model.blue model.users else text ""
    , if model.showHelp then showHelp else text ""
    ]