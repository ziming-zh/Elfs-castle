module Levels exposing (End,Level,GMap,Condition,initLevel1,initLevel2,initLevel3,initMap1,initMap2,initMap3,initEnding1,initEnding2,initEnding3)
import Color exposing (BallColor(..))
import Color exposing (NormalColor(..))
type alias End = 
    { map : GMap
    , pos : (Float,Float)
    }


    -- | Map3 GMap

initLevel1 : Level
initLevel1 = setLevel 1 (initMap1) (6,0,4) 1.0--(6,0,4) 1.0

initLevel2 : Level
initLevel2 = setLevel 2 (initMap2) (7,3,6) 1.0--(7,3,6) 5

initLevel3 : Level
initLevel3 = setLevel 3 (initMap3) (11,8,8) 1.0--(11,8,8) 1.5

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
    [Black, Blue, Purple, Purple,Blue,Black,
     Black, Yellow, Black, Black,Yellow,Black,
     Black, Blue, Yellow, Yellow,Blue,Black,
     Black, Blue, Purple, Purple,Blue,Black]
     (6,4)

initMap2 : GMap
initMap2 = 
    let
        b = Blue
        y = Yellow
        p = Purple
        bl = Black
    in
    
    GMap 
    (List.concat
    [[bl, y, b,  b,  y, bl], 
     [bl, b, bl, p,  b, bl], 
     [b,  p, bl, p,  b, b],
     [b,  b, p,  bl, b, b], 
     [bl, b, b,  bl, b, bl]
     ])
     (6,5)

     
initMap3 : GMap
initMap3 = 
    let
        b = Blue
        y = Yellow
        p = Purple
        bl = Black
    in
    GMap 
    (List.concat
    [[bl , bl, y , b , b , y , bl, bl],
     [bl , p , b , bl, bl, b , p , bl], 
     [b  , b , y , bl, bl, y , b , b ],
     [bl , b , b , y , y , b , b , bl],
     [bl , b , b , p , p , b , b , bl],
     [b  , b , p , bl, bl, p , b , b ],
     [bl , b , p , bl, bl, p , b , bl],
     [bl , b , b , b , b , b , b , bl]
     ])
     (8,8)


initEnding1 : End
initEnding1 = 
    { map =
        GMap 
        (List.concat
        [[Nocolor, Yellow, Yellow, Nocolor],
        [Yellow, Yellow, Yellow, Yellow],
        [Nocolor, Purple, Purple, Nocolor],
        [Nocolor, Purple, Purple, Nocolor]])
        (4,4)
    , pos = (0,0) }

initEnding2 : End
initEnding2 =
    { map = 
        GMap
        (List.concat
        [[Nocolor, Yellow, Yellow, Yellow, Nocolor],
        [Yellow, Yellow, Nocolor, Yellow, Yellow],
        [Nocolor, Purple,Blue, Purple, Nocolor],
        [Nocolor, Purple,Blue, Purple, Nocolor],
        [Nocolor, Purple,Blue, Purple, Nocolor]])
        (5,5)
    , pos = (0,0) }

initEnding3 : End
initEnding3 =
    { map = 
        GMap
        (List.concat
        [[Nocolor, Yellow, Yellow, Yellow, Yellow, Yellow, Nocolor],
        [Yellow, Yellow, Yellow, Nocolor, Yellow, Yellow, Yellow],
        [Nocolor, Purple, Blue, Nocolor, Blue, Purple, Nocolor],
        [Nocolor, Purple, Blue, Nocolor, Blue, Purple, Nocolor],
        [Nocolor, Purple, Blue, Nocolor, Blue, Purple, Nocolor],
        [Nocolor, Purple, Blue, Nocolor, Blue, Purple, Nocolor]])   
        (7,6)
    , pos = (0,0) }