{-
 - Programmer: Peter Salu
 - Created on: December 1, 2015
 - Description: Holds levels for aa.
 -}
module Level where

import Array exposing (Array)

type alias Level =
  { speed : Float
  , initialNumberOfDarts : Int
  , dartsToWin : Int
  }

levels : Array Level
levels =
  Array.fromList
    [ { speed = 0.01
      , initialNumberOfDarts = 0
      , dartsToWin = 3
      }
      ,
      { speed = -0.03
      , initialNumberOfDarts = 2
      , dartsToWin = 3
      }
      ,
      { speed = 0.05
      , initialNumberOfDarts = 5
      , dartsToWin = 5
      }
    ]
