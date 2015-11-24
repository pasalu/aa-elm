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
  Object { radius : Float, numberOfDarts : Int, collisionY : Float }

type alias Dart =
  Object { radius : Float
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
  , collisionY = 0
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
  , radius = 10
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
  , darts = List.repeat 10 defaultDart
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
{-
  let angle' = if direction == Left then
                 angle + angularVelocity * delta
               else
                 angle - angularVelocity * delta
  in
     -}
  { object |
      x <- x + vx * delta
    , y <- y + vy * delta
  }

collidedWithBoard : Dart -> Board -> Bool
collidedWithBoard dart board =
  --dart.y > (board.y - (board.radius + (dart.height / 2) + dart.radius))
  dart.y >= board.collisionY

stepPlayer : Time -> Board -> Bool -> Player -> Player
stepPlayer delta board space player =
  let dartToBeFired' =
        if space || player.isShooting then
          let dartToBeFired =
                if space then
                  player.dartToBeFired + 1
                else
                  player.dartToBeFired
          in
             dartToBeFired
        else
          player.dartToBeFired

      setIsFired = (\index dart ->
                      if index == dartToBeFired' then
                        {dart | isFired <- True}
                      else
                        dart
                   )

      darts' =
        List.indexedMap setIsFired player.darts
          |> List.map (\dart -> stepDart delta dart board)

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
stepDart delta dart board =
  let vy' = if dart.isFired && not dart.collidedWithBoard then 600 else 0
      collidedWithBoard' =
        if not dart.collidedWithBoard then
           collidedWithBoard dart board
        else
          dart.collidedWithBoard

      angle' = dart.angle + if collidedWithBoard' then 0.1 else 0

      (x', y') =
        if dart.collidedWithBoard then
          (board.x + 2 * board.radius * cos angle'
          ,board.y + 2 * board.radius * sin angle'
          )
        else
          (dart.x, dart.y)
      dart' = stepObject
                delta
                {dart |
                        x <- x'
                      , y <- y'
                      , vy <- vy'
                      , angle <- angle'
                      , collidedWithBoard <- collidedWithBoard'
                }
  in
     dart'

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
    b = Debug.watch "Board" board'
    f = Debug.watch "First dart" (List.head player'.darts)
    --w = Debug.watch "Darts" player'.darts
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

--TODO: Remove angle parameter.
displayObject : Float -> Float -> Float -> Form -> Form
displayObject x y angle form =
  move (x, y) form
    |> rotate angle

drawBoard : Board -> Form
drawBoard board =
  group
    [
      (text
        <| Text.height 40
        <| Text.color white
        <| Text.fromString
        <| toString board.numberOfDarts
      )
    , (filled black <| circle board.radius)
    ]

--Draw the board grouping darts that have collided with the board to the board.
displayBoard : Board -> Form
displayBoard board =
  displayObject board.x board.y board.angle <| drawBoard board

dartColor : Color.Color
dartColor = black

drawDart : Dart -> Form
drawDart dart =
  circle dart.radius
    |> filled dartColor

drawLine : Dart -> Form
drawLine dart =
  segment (defaultBoard.x, defaultBoard.y) (dart.x, dart.y)
    |> traced (solid dartColor)

displayDart : Dart -> Form
displayDart dart = displayObject dart.x dart.y 0 (drawDart dart)

display : (Int, Int) -> Game -> Element
display (width, height) {state, board, player} =
  let dartForms = List.map displayDart player.darts

      --Lines for the darts drawn separately so they wont move.
      lineForms =
        List.filter .collidedWithBoard player.darts
          |> List.map drawLine
  in
  container width height middle
    <| collage width height
      <| [ displayBackground width height
         , displayBoard board
         ]
         ++ dartForms
         ++ lineForms

main : Signal Element
main = display <~ Window.dimensions ~ gameState

