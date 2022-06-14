module Model exposing (Model,ArrowKey(..),Plate,Block,Property,Ball,Line,Bricks,State(..),Dir(..),model_init,getBrickPos,model_level1,model_level2,model_level3,initBricks)
import Color exposing (BallColor(..),NormalColor(..))
import Levels exposing (Level,initLevel1,initLevel2,initLevel3)
import Levels exposing (GMap)
import Levels exposing (initEnding1)
import Levels exposing (initEnding2)
import Levels exposing (initEnding3)
type alias Model =
    { time : Float
    , windowsize : ( Float, Float )
    , ball : Ball
    , bricks : Bricks
    , plate : Plate
    , live : Int
    , bounce : Bool
    , state : State
    , score : Int
    , level : Level
    , ending : GMap
    }


type alias Plate = 
    { state : Dir
    , pos : Float
    }

type alias Property = 
    { val : Float
    , max : Float
    }
setProperty : Float -> Float -> Property
setProperty val max =
    Property val max

type alias Ball =
    { pos : (Float, Float)
    , vel : (Float, Float)
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
    | F

type Dir 
    = Left
    | Right
    | None

initBallColor : BallColor
initBallColor = 
    Normal Yellow
    
initBall : Float -> Ball
initBall x = 
    Ball (300, 565) ( x , -x ) (setProperty 0 100) initBallColor (setProperty 0 100) (setProperty 0 100) 

newBall : Ball -> Float -> Ball 
newBall ball x = 
    Ball (300,565) ( x , -x ) ball.energy ball.color ball.mp ball.exp

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

model_init : Int -> Model
model_init x = 
    Model 0 ( 1396, 691 ) (initBall 3) (initBricks initLevel1) initPlate x False Paused 0 initLevel1 initEnding1

model_level1 : Model -> Model
model_level1 model = 
    Model 0 ( 1396, 691 ) (newBall model.ball 3) (initBricks initLevel1) initPlate model.live False Paused 0 initLevel1 initEnding1
    
model_level2 : Model -> Model
model_level2 model = 
    Model 0 ( 1396, 691 ) (newBall model.ball 3.5) (initBricks initLevel2) initPlate model.live False Paused 0 initLevel2 initEnding2

model_level3 : Model -> Model
model_level3 model =
    Model 0 ( 1396, 691 ) (newBall model.ball 4) (initBricks initLevel3) initPlate model.live False Paused 0 initLevel3 initEnding3






