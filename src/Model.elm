module Model exposing (Model,ArrowKey(..),Plate,Block,Property,Ball,Line,Bricks,State(..),Dir(..),model_init,getBrickPos,model_level1,model_level2,model_level3,initBricks,init_model1,init_model2,init_model3)
import Color exposing (BallColor(..),NormalColor(..))
import Levels exposing (Level,initLevel1,initLevel2,initLevel3)
import Levels exposing (GMap,End)
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
    , ending : End 
    , dt : Float
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
    | Changing
    
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
        rows = List.map (\x -> (toFloat x)) (List.map (\x ->  x*50+5) (List.range (round (5- (toFloat sizex)/2)) (round (4+ (toFloat sizex)/2))))
        cols = List.map (\x -> (toFloat x)) (List.map (\x ->  x*50+50) (List.range 0 sizey))
        line =
            \y -> List.map (\x -> Tuple.pair x y) rows
        sol =List.map line cols
            |> List.concat
    in
        zip sol level.map.color  
    
getline : ( Float , Float ) -> ( Float , Float ) -> Int -> List ( Float , Float )
getline (x0,y0) (dx,dy) n = 
    List.map ( \x -> (x0+dx*x,y0+dy*x) ) ( List.map (\x -> (toFloat x)) (List.range 0 (n-1)) )

changeColor : List NormalColor -> List Int -> Int -> List NormalColor
changeColor color list k =
    let
        id = List.head ( List.drop (k-1) list )
    in    
        case id of 
            Just i ->
                changeColor (List.concat [ List.take (i-1) color , [Black] , List.drop i color ] ) list (k+1)
            _ -> color
init_Map1 : Level -> Bricks 
init_Map1 level = 
    let
        l1 = getline (75,50) (100,0) 5
        l2 = getline (125,100) (100,0) 4
        l3 = getline (75,150) (100,0) 5
        l4 = getline (125,200) (100,0) 4
        l5 = getline (75,250) (100,0) 5
        l6 = getline (125,300) (100,0) 4
        l7 = getline (75,350) (100,0) 5
        l8 = getline (175,100) (200,0) 2
        l9 = getline (175,200) (200,0) 2
        l10 = getline (125,150) (100,0) 4
        l11 = getline (175,300) (200,0) 2
        l = List.concat [l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11]
        color = List.concat[List.repeat 32 Grey,List.repeat 4 Purple,List.repeat 6 Yellow]
        lb = [3,7,8,12,21,30,1,5,10,14,19,23,28,32]
        ncolor = changeColor color lb 1
    in
        zip l ncolor

getBrickPos : Bricks -> List (Float, Float)
getBrickPos bricks = 
    List.map (\x -> Tuple.first x) bricks
 
initPlate : Plate
initPlate = Plate None 250

model_init : Int -> Model
model_init x = 
    Model 0 ( 1396, 691 ) (initBall 3) (init_Map1 initLevel1) initPlate x False Paused 0 initLevel1 initEnding1 0

init_model1 : Model -> Model
init_model1 model = 
    Model 0 ( 1396, 691 ) (newBall model.ball 3) (initBricks initLevel1) initPlate model.live False Paused 0 initLevel1 initEnding1 0
    
init_model2 : Model -> Model
init_model2 model = 
    Model 0 ( 1396, 691 ) (newBall model.ball 3.5) (initBricks initLevel2) initPlate model.live False Paused 0 initLevel2 initEnding2 0

init_model3 : Model -> Model
init_model3 model =
    Model 0 ( 1396, 691 ) (newBall model.ball 4) (initBricks initLevel3) initPlate model.live False Paused 0 initLevel3 initEnding3 0

model_level1 : Model -> Model
model_level1 model = 
    { model | ball =  (newBall model.ball 3) , state = Paused } 

model_level2 : Model -> Model
model_level2 model = 
    { model | ball =  (newBall model.ball 3.5) , state = Paused } 

model_level3 : Model -> Model
model_level3 model =    
    { model | ball =  (newBall model.ball 4) , state = Paused } 





