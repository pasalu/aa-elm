{-
 - Programmer: Peter Salu
 - Created on: August 6, 2015
 - Description: Clone of aa the game.
 - https://play.google.com/store/apps/details?id=com.aa.generaladaptiveapps&hl=en
 -}

module Aa where

import Graphics.Element exposing (..)
import Graphics.Collage exposing
  (Form, Shape, move, moveY, filled, rect, circle, collage, group)
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
type alias Object a =
  { a |
      x : Float
    , y : Float
    , vx : Float
    , vy : Float
  }

type Direction = Left | Right

type alias Board =
  Object { radius : Float, numberOfDarts : Int, direction : Direction }

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

defaultDart : Dart
defaultDart =
  { x = 0
  , y = -300
  , vx = 0
  , vy = 0
  , height = 70
  , width = 1
  , radius = 5
  }

defaultPlayer : Player
defaultPlayer =
  { x = 0, y = 0, vx = 0, vy = 0, darts = [defaultDart], isShooting = False }

defaultGame : Game
defaultGame =
  { state = Pause
  , board = { x = 0,
              y = 130,
              vx = 0,
              vy = 0,
              radius = 100,
              numberOfDarts = 0,
              direction = Left
            }
  , player = defaultPlayer
  }

--Update the game.
stepObject : Time -> Object a -> Object a
stepObject time ({x, y, vx, vy} as object) =
  { object |
      x <- x + vx * time,
      y <- y + vy * time
  }

collidedWithBoard : Dart -> Board -> Bool
collidedWithBoard dart board =
  dart.y > (board.y - (board.radius + (dart.height / 2) + dart.radius))

stepPlayer : Time -> Board -> Bool -> Player -> Player
stepPlayer delta board isShooting player =
  let
    darts' =
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
        List.any (\collidedStatus -> collidedStatus == True)
          <| List.map check darts
  in
     collided

stepDart : Time -> Dart -> Board -> Dart
stepDart time ({x, y, vx, vy} as dart) board =
  let dart' = stepObject time {dart | vy <- 500}
      y' = if collidedWithBoard dart board then dart.y else dart'.y
  in
     {dart' | y <- y'}

stepGame : Input -> Game -> Game
stepGame input game =
  let
    {space, enter, delta} = input
    {state, board, player} = game

    state' =
      if | enter -> Play
         | otherwise -> state

    player' = stepPlayer delta board space player
  in
     {game | state <- state', player <- player'}

gameState : Signal Game
gameState = Signal.foldp stepGame defaultGame input

-- View for the game.
displayBackground : Int -> Int -> Form
displayBackground width height =
  filled yellow (rect (toFloat width) (toFloat height))

displayBoard : Board -> Form
displayBoard board =
  move (board.x, board.y) (filled black (circle board.radius))

drawDart : Dart -> Form
drawDart dart =
    group
      [ (moveY -(dart.height / 2) <| filled white <| circle dart.radius)
      , (filled white <| rect dart.width dart.height)
      ]

displayDart : Dart -> Form
displayDart dart = move (dart.x, dart.y) (drawDart dart)

display : (Int, Int) -> Game -> Element
display (width, height) {state, board, player} =
  container width height middle
    <| collage width height
      <| [ displayBackground width height
         , displayBoard board
         ]
         ++ List.map displayDart player.darts

main = display <~ Window.dimensions ~ gameState
