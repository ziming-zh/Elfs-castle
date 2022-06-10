module Levels exposing (..)
import Color exposing (BallColor(..))
import Color exposing (NormalColor(..))


    -- | Map3 GMap
type GameLevel
    = Level1 Level
    | Level2 Level

initLevel1 : Level
initLevel1 = setLevel ( initMap1) (1,1,1) 1.0

initLevel2 : Level
initLevel2 = setLevel ( initMap2) (2,2,3) 1.2

type alias Condition = (Int, Int, Int) -- Purple, Yellow, Blue Required
type alias GMap = 
    { color : List NormalColor
    , size : (Int, Int) 
    }
type alias Level = 
    {  map : GMap
    , pass : Condition
    , speed : Float
    }
setLevel : GMap -> Condition -> Float -> Level
setLevel map condition speed=
    Level map condition speed

initMap1 : GMap
initMap1 = 
    GMap 
    [Yellow, Yellow, Blue, Purple,
     Yellow, Blue, Purple, Yellow,
     Blue, Purple, Yellow, Blue,
     Purple, Yellow, Blue, Yellow]
     (4,4)

initMap2 : GMap
initMap2 = 
    GMap 
    [Purple, Yellow, Blue, Purple, Yellow, Blue, 
     Blue, Purple, Yellow, Blue, Purple, Yellow, 
     Purple, Yellow, Blue, Purple, Yellow, Blue,
     Yellow, Blue, Purple, Yellow, Blue, Purple, 
     Blue, Purple, Yellow, Blue, Purple, Blue,
     Purple, Yellow, Blue, Purple, Yellow, Blue
     ]
     (6,6)