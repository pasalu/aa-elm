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
import List exposing (map)
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
    <| Input <~ Keyboard.space ~ Keyboard.enter ~ delta

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
  Object { radius : Float, numberOfDarts : Int }

type alias Dart =
  Object { height : Float, width : Float, radius : Float }

type alias Player =
  Object { darts : List Dart, isShooting : Bool }

type State = Play | Pause

type alias Game =
  { state : State
  , board : Board
  , player : Player
  }

{-
-- Not used because fields can't be updated AND inserted.
defaultObject : Object
defaultObject =
  { x = 0
  , y = 0
  , vx = 0
  , vy = 0
  , angle = 0
  , angularVelocity = 0
  , direction = Right
  }
  -}

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
  }

defaultGame : Game
defaultGame =
  { state = Pause
  , player = defaultPlayer
  , board = defaultBoard
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
  dart.y > (board.y - (board.radius + (dart.height / 2) + dart.radius))

stepPlayer : Time -> Board -> Bool -> Player -> Player
stepPlayer delta board isShooting player =
  let darts' =
        if isShooting || player.isShooting then
          List.map (\dart -> stepDart delta dart board) player.darts
        else
          player.darts
      isShooting' = isShooting || anyCollidedWithBoardOrInFlight darts' board
  in
    { player | darts <- darts', isShooting <- isShooting' }

anyCollidedWithBoardOrInFlight : List Dart -> Board -> Bool
anyCollidedWithBoardOrInFlight darts board =
  let check = (\dart -> collidedWithBoard dart board || dart.y > defaultDart.y)
      collided =
        List.any (\status -> status == True)
          <| List.map check darts
  in
     collided

stepDart : Time -> Dart -> Board -> Dart
stepDart delta ({x, y, vx, vy} as dart) board =
  let dart' = stepObject delta {dart | vy <- 500}
      (y', angle') = if collidedWithBoard dart board then
                       (dart.y, board.angle)
                     else
                       (dart'.y, dart'.angle)
  in
     {dart' | y <- y', angle <- angle'}

stepBoard : Time -> Board -> Board
stepBoard delta board =
  stepObject delta board

stepGame : Input -> Game -> Game
stepGame input game =
  let
    {space, enter, delta} = input
    {state, board, player} = game

    state' =
      if | enter -> Play
         | otherwise -> state

    board' = stepBoard delta board
    player' = stepPlayer delta board' space player
  in
     {game | state <- state', player <- player', board <- board'}

gameState : Signal Game
gameState = Signal.foldp stepGame defaultGame input

-- View for the game.
displayBackground : Int -> Int -> Form
displayBackground width height =
  filled white (rect (toFloat width) (toFloat height))

displayObject : Float -> Float -> Float -> Form -> Form
displayObject x y angle form =
  form
    |> move (x, y)
    |> rotate angle

displayBoard : Board -> Form
displayBoard board =
  displayObject board.x board.y board.angle
    <| group [ (filled black <| circle board.radius)
             , (text
                  <| Text.height 40
                  <| Text.color white
                  <| Text.fromString
                  <| toString board.numberOfDarts
                )
             ]

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
  container width height middle
    <| collage width height
      <| [ displayBackground width height
         , displayBoard board
         ]
         ++ List.map displayDart player.darts

main = display <~ Window.dimensions ~ gameState
