module Levels exposing (Level,GMap,Condition,initLevel1,initLevel2,initLevel3,initMap1,initMap2,initMap3,initEnding1,initEnding2,initEnding3)
import Color exposing (BallColor(..))
import Color exposing (NormalColor(..))


    -- | Map3 GMap

initLevel1 : Level
initLevel1 = setLevel 1 (initMap1) (6,0,4) 1.0

initLevel2 : Level
initLevel2 = setLevel 2 (initMap2) (3,7,6) 5

initLevel3 : Level
initLevel3 = setLevel 3 (initMap3) (8,11,8) 1.5

type alias Condition = (Int, Int, Int) --Yellow, Blue, Purple Required
type alias GMap = 
    { color : List NormalColor
    , size : (Int, Int) 
    }
type alias Level = 
    { id : Int 
    , map : GMap
    , pass : Condition
    , speed : Float
    }


setLevel : Int -> GMap -> Condition -> Float -> Level
setLevel id map condition speed=
    Level id map condition speed

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

     
initMap3 : GMap
initMap3 = 
    GMap 
    [Yellow,Purple, Yellow, Blue, Purple, Yellow, Blue, Purple,
     Purple,Blue, Purple, Yellow, Blue, Purple, Yellow, Blue,
     Yellow,Purple, Yellow, Blue, Purple, Yellow, Blue,Purple,
     Blue,Yellow, Blue, Purple, Yellow, Blue, Purple,Yellow, 
     Yellow,Blue, Purple, Yellow, Blue, Purple, Blue,Purple,
     Blue,Purple, Yellow, Blue, Purple, Yellow, Blue,Yellow,
     Purple,Blue,Yellow,Blue,Purple,Yellow,Blue,Purple,
     Yellow,Purple,Blue,Yellow,Blue,Purple,Yellow,Blue
     ]
     (8,8)


initEnding1 : GMap
initEnding1 = 
    GMap 
        [Nocolor, Yellow, Yellow, Nocolor,
        Yellow, Yellow, Yellow, Yellow,
        Nocolor, Purple, Purple, Nocolor,
        Nocolor, Purple, Purple, Nocolor]
        (4,4)

initEnding2 : GMap
initEnding2 =
    GMap
        [Nocolor, Yellow, Yellow, Yellow, Nocolor,
        Yellow, Yellow, Nocolor, Yellow, Yellow,
        Nocolor, Purple,Blue, Purple, Nocolor,
        Nocolor, Purple,Blue, Purple, Nocolor,
        Nocolor, Purple,Blue, Purple, Nocolor]
        (5,5)

initEnding3 : GMap
initEnding3 =
    GMap
        [Nocolor, Yellow, Yellow, Yellow, Yellow, Yellow, Nocolor,
        Yellow, Yellow, Yellow, Nocolor, Yellow, Yellow, Yellow,
        Nocolor, Purple, Blue, Nocolor, Blue, Purple, Nocolor,
        Nocolor, Purple, Blue, Nocolor, Blue, Purple, Nocolor,
        Nocolor, Purple, Blue, Nocolor, Blue, Purple, Nocolor,
        Nocolor, Purple, Blue, Nocolor, Blue, Purple, Nocolor]
        (7,6)