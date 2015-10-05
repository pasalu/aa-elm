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

--Inputs
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

main = show <~ input
