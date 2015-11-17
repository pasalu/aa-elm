{-
 - Programmer: Peter Salu
 - Created on: August 6, 2015
 - Description: Clone of aa the game.
 - https://play.google.com/store/apps/details?id=com.aa.generaladaptiveapps&hl=en
 -}

module Aa where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Text
import Color exposing (black, white, yellow)
import Signal exposing ((<~), (~))
import Time exposing (Time, fps, inSeconds)
import Keyboard
import Window
import List
import Debug

--Inputs to the game.
type alias Input =
  { space : Bool
  , enter : Bool
  , delta : Time
  }

delta : Signal Time
delta = inSeconds <~ (fps 60)

input : Signal Input
input =
  Signal.sampleOn delta
    <| Input <~ Signal.dropRepeats Keyboard.space ~ Keyboard.enter ~ delta

--Models of the game.
type Direction = Left | Right

type alias Object a =
  { a |
      x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , angle : Float
    , angularVelocity : Float
    , direction : Direction
  }

type alias Board =
  Object { radius : Float, numberOfDarts : Int, collisionY : Float }

type alias Dart =
  Object { height : Float
         , width : Float
         , radius : Float
         , isFired : Bool
         , collidedWithBoard : Bool
         }

type alias Darts = List Dart

type alias Player =
  Object { darts : Darts, isShooting : Bool, dartToBeFired : Int }

type State = Play | Pause

type alias Game =
  { state : State
  , board : Board
  , player : Player
  , spaceCount : Int
  }

defaultBoard : Board
defaultBoard =
  { x = 0
  , y = 130
  , vx = 0
  , vy = 0
  , angle = 0
  , angularVelocity = 5
  , direction = Left
  , radius = 100
  , numberOfDarts = 10
  , collisionY = -100
  }

defaultDart : Dart
defaultDart =
  { x = 0
  , y = -300
  , vx = 0
  , vy = 0
  , angle = 0
  , angularVelocity = 0
  , direction = Right
  , height = 70
  , width = 1
  , radius = 5
  , isFired = False
  , collidedWithBoard = False
  }

defaultPlayer : Player
defaultPlayer =
  { x = 0
  , y = 0
  , vx = 0
  , vy = 0
  , angle = 0
  , angularVelocity = 0
  , direction = defaultBoard.direction
  , darts = List.repeat 100 defaultDart
  , isShooting = False
  , dartToBeFired = -1
  }

defaultGame : Game
defaultGame =
  { state = Pause
  , player = defaultPlayer
  , board = defaultBoard
  , spaceCount = 0
  }

--Update the game.
stepObject : Time -> Object a -> Object a
stepObject delta ({x, y, vx, vy, angle, angularVelocity, direction} as object) =
  let angle' = if direction == Left then
                 angle + angularVelocity * delta
               else
                 angle - angularVelocity * delta
  in
  { object |
      x <- x + vx * delta
    , y <- y + vy * delta
    , angle <- angle'
  }

collidedWithBoard : Dart -> Board -> Bool
collidedWithBoard dart board =
  --dart.y > (board.y - (board.radius + (dart.height / 2) + dart.radius))
  dart.y >= board.collisionY

stepPlayer : Time -> Board -> Bool -> Player -> Player
stepPlayer delta board space player =
  let (dartToBeFired', darts') =
        if space || player.isShooting then
          let dartToBeFired =
                if space then
                  player.dartToBeFired + 1
                else
                  player.dartToBeFired

              setIsFired = (\index dart ->
                              if index == dartToBeFired then
                                {dart | isFired <- True}
                              else
                                dart
                           )

              darts =
                List.indexedMap setIsFired player.darts
                  |> List.map (\dart -> stepDart delta dart board)
          in
             (dartToBeFired, darts)
        else
          (player.dartToBeFired, player.darts)

      isShooting' = space || anyInFlight darts' board
  in
    { player |
               darts <- darts'
             , isShooting <- isShooting'
             , dartToBeFired <- dartToBeFired'
    }

anyInFlight : Darts -> Board -> Bool
anyInFlight darts board =
  let check =
        (\dart -> not dart.collidedWithBoard && dart.y > defaultDart.y)
      collided =
        List.map check darts
          |> List.any ((==) True)
  in
     collided

stepDart : Time -> Dart -> Board -> Dart
stepDart delta ({x, y, vx, vy, isFired} as dart) board =
  let vy' = if isFired then 500 else 0
      dart' = stepObject delta {dart | vy <- vy'}
      collidedWithBoard' = collidedWithBoard dart' board
      (x', y', angle') =
        if collidedWithBoard' then
          --(dart'.y, dart'.y, dart'.angle)
          --(20 * cos board.angle, 20 * sin board.angle, board.angle)
          (dart.x, dart.y, dart.angle)
        else
          (dart'.x, dart'.y, dart'.angle)
  in
     {dart' |
             x <- x'
            , y <- y'
            , angle <- angle'
            , collidedWithBoard <- collidedWithBoard'
     }

stepBoard : Time -> Board -> Board
stepBoard delta board =
  stepObject delta board

stepGame : Input -> Game -> Game
stepGame input game =
  let
    {space, enter, delta} = input
    {state, board, player, spaceCount} = game

    state' =
      if | enter -> Play
         | otherwise -> state

    (spacePressed, spaceCount') =
      if space then
         if spaceCount == 0 then
            (space, spaceCount + 1)
         else
            (False, spaceCount + 1)
      else
        (space, 0)

    board' = stepBoard delta board
    player' = stepPlayer delta board' spacePressed player
    sp = Debug.watch "Space Pressed" spacePressed
  in
     {game |
             state <- state'
           , player <- player'
           , board <- board'
           , spaceCount <- spaceCount'
     }

gameState : Signal Game
gameState = Signal.foldp stepGame defaultGame input

-- View for the game.
displayBackground : Int -> Int -> Form
displayBackground width height =
  filled white (rect (toFloat width) (toFloat height))

displayObject : Float -> Float -> Float -> Form -> Form
displayObject x y angle form =
  move (x, y) form
    |> rotate angle

drawBoard : Board -> Form
drawBoard board =
  group
    [ (filled black <| circle board.radius)
    , (text
        <| Text.height 40
        <| Text.color white
        <| Text.fromString
        <| toString board.numberOfDarts
      )
    ]

--Draw the board grouping darts that have collided with the board to the board.
displayBoard : Board -> Darts -> Form
displayBoard board darts =
--(\dart -> (drawDart dart |> moveY -100))
  let dartFun = (\dart -> displayDart dart)
      dartsCollidedWithBoardForm = List.map dartFun darts
  in
  displayObject
    board.x
    board.y
    board.angle
    (group <| {- [drawBoard board] ++ -}dartsCollidedWithBoardForm)

drawDart : Dart -> Form
drawDart dart =
  group
      [ (moveY -(dart.height / 2) <| filled black <| circle dart.radius)
      , (filled black <| rect dart.width dart.height)
      ]

displayDart : Dart -> Form
displayDart dart = displayObject dart.x dart.y dart.angle <| (drawDart dart)

display : (Int, Int) -> Game -> Element
display (width, height) {state, board, player} =
  let (dartsCollidedWithBoard, dartsNotCollidedWithBoard) =
        List.partition .collidedWithBoard player.darts
      dartNotCollidedWithBoardForms =
        List.map displayDart dartsNotCollidedWithBoard
  in
  container width height middle
    <| collage width height
      <| [ displayBackground width height
         , displayBoard board dartsCollidedWithBoard
         ]
         ++ dartNotCollidedWithBoardForms

main = display <~ Window.dimensions ~ gameState

