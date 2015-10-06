{-
 - Programmer: Peter Salu
 - Created on: August 6, 2015
 - Description: Clone of aa the game.
 - https://play.google.com/store/apps/details?id=com.aa.generaladaptiveapps&hl=en
 -}

module Aa where

import Graphics.Element exposing (show)
import Signal exposing ((<~), (~))
import Time exposing (Time, fps, inSeconds)
import Keyboard

--Inputs to the game.
type alias Input =
  { space : Bool
  , delta : Time
  }

delta : Signal Time
delta = inSeconds <~ (fps 60)

input : Signal Input
input =
  Signal.sampleOn delta <|
    Input <~ Keyboard.space ~ delta

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
  Object { numberOfDarts : Int, direction : Direction }

type alias Dart =
  Object {}

type alias Player =
  Object { darts : List Dart }

type State = Play | Pause

type alias Game =
  { state : State
  , board : Board
  , player : Player
  }

defaultGame : Game
defaultGame =
  { state = Pause
  , board = { x = 0,
              y = 0,
              vx = 1,
              vy = 1,
              numberOfDarts = 0,
              direction = Left
            }
  , player = { x = 0, y = 10, vx = 0, vy = 0, darts = [] }
  }


main = show defaultGame
