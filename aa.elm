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
import Color exposing (black, white, yellow, red)
import Signal exposing ((<~), (~))
import Time exposing (Time, fps, inSeconds)
import Keyboard
import Window
import List
import Array exposing (Array)
import Debug

import Level


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
         , collidedWithOtherDart : Bool
         , collidedSpeed : Float
         }

type alias Darts = List Dart

type alias Player =
  Object { darts : Darts, isShooting : Bool, indexOfDartToBeFired : Int }

type State = LoadLevelWin | LoadLevelLose | Play | Pause

type alias Game =
  { state : State
  , board : Board
  , player : Player
  , spaceCount : Int
  , level : Int
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
  , collisionY = -85
  }

defaultDart : Dart
defaultDart =
  { x = 0
  , y = -300
  , vx = 0
  , vy = 0
  , angle = (3 * pi) / 2 --270 degrees in radians.
  , angularVelocity = 0
  , direction = Right
  , radius = 10
  , isFired = False
  , collidedWithBoard = False
  , collidedWithOtherDart = False
  , collidedSpeed = 0
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
  , darts = []
  , isShooting = False
  , indexOfDartToBeFired = -1
  }

defaultGame : Game
defaultGame =
  { state = LoadLevelLose
  , player = defaultPlayer
  , board = defaultBoard
  , spaceCount = 0
  , level = 0
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

unsafeGet : Int -> Array a -> a
unsafeGet index array =
  case Array.get index array of
    Just item ->
      item
    Nothing ->
      Debug.crash "Unknown Index"

stepPlayer : Time -> Board -> Bool -> Player -> Player
stepPlayer delta board space player =
  let indexOfDartToBeFired' =
        if space || player.isShooting then
          let indexOfDartToBeFired =
                if space then
                  player.indexOfDartToBeFired + 1
                else
                  player.indexOfDartToBeFired
          in
             indexOfDartToBeFired
        else
          player.indexOfDartToBeFired

      dartsArray =
        Array.fromList player.darts
          |> Array.map (\dart -> stepDart delta dart board)
      dartsArray' =
        if (space || player.isShooting)
           && indexOfDartToBeFired' > defaultPlayer.indexOfDartToBeFired
           && indexOfDartToBeFired' < Array.length dartsArray then
          let dartToBeFired = unsafeGet indexOfDartToBeFired' dartsArray
              dartsArrayWithFired =
                Array.set
                  indexOfDartToBeFired'
                  {dartToBeFired | isFired <- True}
                  dartsArray
          in
            collidedWithOtherDarts dartToBeFired dartsArrayWithFired
        else
          dartsArray
      darts' = Array.toList dartsArray'

      isShooting' = space || anyInFlight darts' board
  in
    { player |
               darts <- darts'
             , isShooting <- isShooting'
             , indexOfDartToBeFired <- indexOfDartToBeFired'
    }

dartsInFlight : Darts -> Board -> Darts
dartsInFlight darts board =
  let check dart = not dart.collidedWithBoard && dart.y > defaultDart.y
  in
    List.filter check darts

anyInFlight : Darts -> Board -> Bool
anyInFlight darts board =
  dartsInFlight darts board
    |> List.length
    |> (/=) 0

collidedWithOtherDarts : Dart -> Array Dart -> Array Dart
collidedWithOtherDarts dart darts =
  let collided aDart =
    let dartDistance = distance aDart.x aDart.y dart.x dart.y
    in
      if aDart.collidedWithBoard
         && aDart /= dart
         && dartDistance <= dart.radius * 2 then
        {aDart | collidedWithOtherDart <- True}
      else
        aDart
  in
    Array.map collided darts

distance : Float -> Float -> Float -> Float -> Float
distance x1 y1 x2 y2 =
  sqrt <| ((x2 - x1) ^ 2) + ((y2 - y1) ^ 2)

stepDart : Time -> Dart -> Board -> Dart
stepDart delta dart board =
  let vy' = if dart.isFired && not dart.collidedWithBoard then 600 else 0
      collidedWithBoard' =
        if not dart.collidedWithBoard then
           collidedWithBoard dart board
        else
          dart.collidedWithBoard

      angle' = dart.angle + if collidedWithBoard' then dart.collidedSpeed else 0

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

initialBoardDarts : Int -> Float -> Darts
initialBoardDarts n collidedSpeed =
  let delta = (2 * pi) / toFloat n --360 degrees divided by n.
      nDeltas = List.repeat n delta
      angles = List.scanl (+) 0 nDeltas
      updateAngle dart angle =
        {dart | angle <- angle
              , collidedWithBoard <- True
              , collidedSpeed <- collidedSpeed
        }
      defaultDarts = List.repeat n defaultDart
  in
    List.map2 updateAngle defaultDarts angles

stepBoard : Time -> Board -> Board
stepBoard delta board =
  stepObject delta board

loadLevel : Game -> Game
loadLevel game =
  let levelToLoad =
        if game.state == LoadLevelWin then
          game.level + 1
        else
          game.level
      level = unsafeGet levelToLoad Level.levels

      initialNumberOfDarts = level.initialNumberOfDarts
      dartsToWin = level.dartsToWin
      speed = level.speed

      indexOfDartToBeFired' = initialNumberOfDarts - 1
      darts' = initialBoardDarts initialNumberOfDarts speed
                 ++ List.repeat
                      dartsToWin
                      {defaultDart | collidedSpeed <- speed}

      player' =
        {defaultPlayer |
                         darts <- darts'
                       , indexOfDartToBeFired <- indexOfDartToBeFired'
        }
  in
    {game |
            player <- player'
          , level <- levelToLoad
    }

stepGame : Input -> Game -> Game
stepGame input game =
  let
    {space, enter, delta} = input

    game' =
      if game.state == LoadLevelWin || game.state == LoadLevelLose then
        loadLevel game
      else
        game
    {state, board, player, spaceCount} = game'

    state' =
      case state of
        LoadLevelWin -> Play
        LoadLevelLose -> Play
        _ -> state

    --TODO: Move this into a function.
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
    darts = player'.darts

    dartsNotOnBoard = List.filter (\dart -> not dart.collidedWithBoard) darts
    dartsCollidedWithOtherDart = List.filter .collidedWithOtherDart darts

    playerState =
      if dartsCollidedWithOtherDart /= [] then
        LoadLevelLose
      else if dartsNotOnBoard == [] then
        LoadLevelWin
      else
        state'
    dc = Debug.watch "Collided" dartsCollidedWithOtherDart
    --w = Debug.watch "Game" game'
  in
     {game' |
              state <- playerState
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

displayObject : Float -> Float -> Form -> Form
displayObject x y form =
  move (x, y) form

drawBoard : Board -> Form
drawBoard board =
  filled black <| circle board.radius

--Draw the board grouping darts that have collided with the board to the board.
displayBoard : Board -> Form
displayBoard board =
  displayObject board.x board.y <| drawBoard board

dartColor : Color.Color
dartColor = black

drawDart : Dart -> Form
drawDart dart =
  let drawDartColor = if dart.collidedWithOtherDart then red else dartColor
  in
  circle dart.radius
    |> filled drawDartColor

drawLine : Dart -> Form
drawLine dart =
  segment (defaultBoard.x, defaultBoard.y) (dart.x, dart.y)
    |> traced (solid dartColor)

displayDart : Dart -> Form
displayDart dart = displayObject dart.x dart.y (drawDart dart)

display : (Int, Int) -> Game -> Element
display (width, height) {board, player} =
  let dartForms = List.map displayDart player.darts

      --Lines for the darts drawn separately so they wont move when the darts
      --are relocated.
      lineForms =
        List.filter .collidedWithBoard player.darts
          |> List.map drawLine
  in
  container width height middle
    <| collage width height
      <| displayBackground width height
         :: lineForms
         ++ dartForms
         ++ [ displayBoard board ]

main : Signal Element
main = display <~ Window.dimensions ~ gameState

