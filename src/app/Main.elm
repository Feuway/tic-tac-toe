import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

import Array
import Tuple
import Task


-- MAIN

main =
  Browser.element
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = view
  }

-- MODEL

type alias Model = 
  { gameStatus : GameStatus
  , player : Player
  , boardSize : Int
  , board : Board
  }

type GameStatus
  = Preparing
  | Playing Player
  | Winning Player
  | Draw

type Player
  = One
  | Two

type Symbol
  = X
  | O
  | Empty

type alias Board =
  Array.Array Symbol

defaultSize : Int
defaultSize =
  3

initModel =
  Model
    Preparing
    One
    defaultSize
    (Array.initialize (defaultSize ^ 2) (always Empty))


init : () -> ( Model, Cmd Msg )
init _ =
  ( initModel, Cmd.none )


-- UPDATE

type Msg 
  = SizeChanged String
  | PlayerSelected Player
  | GameStarted
  | CellSelected Int
  | TurnEnded GameStatus
  | GameRestarted

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    SizeChanged size ->
      case String.toInt size of
        Just s ->
          let 
            validSize = if s <= 3 then 3 else s
          in
            (
              { model
                | boardSize = validSize
                , board = Array.initialize (validSize ^ 2) (always Empty)
              }
              , Cmd.none
            )
        Nothing ->
          (model, Cmd.none)

    PlayerSelected a ->
      ({ model | player = a }, Cmd.none)

    GameStarted ->
      ({ model | gameStatus = Playing model.player}, Cmd.none)

    CellSelected i -> 
      let
        newModel = { model | board = Array.set i (getSymbolPlayer model.player) model.board }
      in
      ( newModel
        , Task.perform TurnEnded (Task.succeed (checkStatus newModel))
      )
    
    TurnEnded status ->
      (
        { 
        model
          | player = switchUser model.player
          , gameStatus = status
        }
        , Cmd.none
      )

    GameRestarted ->
      (
        { model
          | gameStatus = Playing model.player
          , board = Array.initialize (model.boardSize ^ 2) (always Empty)
        }
        , Cmd.none
      )
       

switchUser : Player -> Player
switchUser player =
  case player of
    One ->
      Two

    Two ->
      One

getSymbolPlayer : Player -> Symbol
getSymbolPlayer player =
  case player of
    One ->
      X

    Two ->
      O

checkStatus : Model -> GameStatus
checkStatus { board, player, boardSize } =
  if (checkRows board boardSize player) || (checkCols board boardSize player) || (checkDiagonals board boardSize player) then
    Winning player
  else if Array.isEmpty <| Array.filter ((==) Empty) board then
    Draw
  else
    Playing (switchUser player)

checkRows : Board -> Int -> Player -> Bool
checkRows board size player =
  (\n (i, _) -> (((n * size) <= i) && (i < (n * size + size))))
    |> collectCellsBy board size 
    |> checkCells player

checkCols : Board -> Int -> Player -> Bool
checkCols board size player =
  (\n (i, _) -> modBy size (i - n) == 0)
    |> collectCellsBy board size
    |> checkCells player

collectCellsBy : Board -> Int -> (Int -> (Int, Symbol) -> Bool) -> List (List (Int, Symbol))
collectCellsBy board size predicate =
  (\n -> List.filter (predicate n) (Array.toIndexedList board))
    |> Array.initialize size
    |> Array.toList

checkCells : Player -> List (List (Int, Symbol)) -> Bool
checkCells player cells =
  List.any (List.all ((getSymbolPlayer player |> (==)) << Tuple.second)) cells

checkDiagonals : Board -> Int -> Player -> Bool
checkDiagonals board size player =
  let
    length = Array.length board
    firstD = findDiagonal 0 (length - 1) size board
    secondD = findDiagonal (size - 1) (length - size) size board
  in
    checkCells player [firstD, secondD]

findDiagonal : Int -> Int -> Int -> Board -> List (Int, Symbol)
findDiagonal start end size board =
  let
    step = (end - start) // (size - 1)
    indexedList = List.repeat size start |> List.indexedMap (\i n -> n + i * step)
  in
    board
      |> Array.toIndexedList 
      |> List.filter (\(i, _) -> List.member i indexedList)

getStatusMessage: GameStatus -> String
getStatusMessage status = 
  case status of
    Preparing ->
        "Play now!"

    Playing One ->
        "Current round: Player X"

    Playing Two ->
        "Current round: Player O"

    Winning One ->
        "Player X won!"

    Winning Two ->
        "Player O won!"

    Draw ->
        "Draw!"

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW

view : Model -> Html Msg
view model =
  div [class "layout"]
    [ h1 [] [text "Tic Tac Toe"]
    , div [class "panel"]
      [ case model.gameStatus of
          Playing a ->
            div [class "som"]
            [ text <| getStatusMessage model.gameStatus
            , button [onClick <| GameRestarted] [text "Restart"]
            ]
          _ ->
            viewSettings model
      ]
    , div [class "game"]
      [ div [class "board", style "width" ((String.fromInt (model.boardSize * 150)) ++ "px")] <| viewCells model.board
      , case model.gameStatus of
          Preparing ->
            viewMessageScreen (getStatusMessage model.gameStatus) (button [onClick GameStarted] [text "Start game"])

          Playing a ->
            div [] []

          Winning a ->
            viewWinningScreen <| getStatusMessage model.gameStatus
          
          Draw -> 
            viewWinningScreen <| getStatusMessage model.gameStatus
      ]
    ]

viewCells : Board -> List (Html Msg)
viewCells board =
  board
    |> Array.indexedMap
      (\i cell ->
        case cell of
          Empty ->
            div [class "cell empty", onClick <| CellSelected i] [text <| viewSymbol cell]
          X ->
            div [class "cell player-one"] [text <| viewSymbol cell]
          O ->
            div [class "cell player-two"] [text <| viewSymbol cell]
      )
    |> Array.toList

viewSymbol : Symbol -> String
viewSymbol symbol =
  case symbol of
    X ->
      "X"

    O ->
      "O"
      
    Empty ->
      ""

viewSettings : Model -> Html Msg
viewSettings model =
  div []
    [ label []
      [ text "Select size of game board: "
      , input [type_ "number", value (String.fromInt model.boardSize), onInput SizeChanged] []
      ]
    , div [class "select-player"]
      [ text "Select player: "
      , button [class "player-btn", onClick <| PlayerSelected One] [text <| viewSymbol X]
      , button [class "player-btn", onClick <| PlayerSelected Two] [text <| viewSymbol O]
      ]
    ]

viewMessageScreen : String -> Html Msg -> Html Msg
viewMessageScreen msg slot =
  div [class "message-screen"]
    [ text msg
    , slot
    ]

viewWinningScreen : String -> Html Msg
viewWinningScreen msg =
  viewMessageScreen msg <| button [onClick <| GameRestarted] [text "Restart"]
  