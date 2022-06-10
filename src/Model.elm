module Model exposing (..)
import Color exposing (Color,BallColor(..),NormalColor(..))
import Levels exposing (..)
type alias Model =
    { time : Float
    , windowsize : ( Float, Float )
    , ball : Ball
    , bricks : Bricks
    , plate : Plate
    , bounce : Bool
    , state : State
    , score : Int
    }


type alias Plate = 
    { state : Dir
    , pos : Float
    }

type alias Property = 
    { val : Int
    , max : Int
    }
setProperty : Int -> Int -> Property
setProperty val max =
    Property val max

type alias Ball =
    { pos : (Float, Float)
    , vel : (Float, Float)
    , live : Int
    , energy : Property
    , color : BallColor
    , mp : Property
    , exp : Property
    }
type alias Line = 
    { p1 : (Float,Float)
    , p2 : (Float,Float)
    }



type alias Bricks = 
    List Block

type alias Block = 
    (( Float , Float ) , NormalColor)
    
type State
    = Paused
    | Playing
    | GG
    
type ArrowKey
    = NoKey
    | Space
    | LeftKey
    | RightKey
    | UpKey
    | DownKey

type Dir 
    = Left
    | Right
    | None

initBallColor : BallColor
initBallColor = 
    Normal Yellow
    
initBall : Ball
initBall = 
    Ball (300, 565) ( 3 , -3 ) 2 (setProperty 100 100) initBallColor (setProperty 50 50) (setProperty 50 50) 
zip : List a -> List b -> List (a, b)
zip xs ys =
  List.map2 Tuple.pair xs ys

initBricks : Level -> Bricks
initBricks level = 
    let
        sizex = Tuple.first level.map.size
        sizey = Tuple.second level.map.size
        rows = List.map (\x -> (toFloat x)) (List.map (\x ->  x*60+5) (List.range (round (5- (toFloat sizex)/2)) (round (4+ (toFloat sizex)/2))))
        cols = List.map (\x -> (toFloat x)) (List.map (\x ->  x*60+50) (List.range 0 sizey))
        line =
            \y -> List.map (\x -> Tuple.pair x y) rows
        sol =List.map line cols
            |> List.concat
    in
        zip sol level.map.color  

getBrickPos : Bricks -> List (Float, Float)
getBrickPos bricks = 
    List.map (\x -> Tuple.first x) bricks

initPlate : Plate
initPlate = Plate None 250


model_init : Model
model_init = 
    
    Model 0 ( 1396, 691 ) initBall (initBricks initLevel2) initPlate False Playing 0





