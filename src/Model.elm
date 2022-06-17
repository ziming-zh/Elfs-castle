module Model exposing (Model,ArrowKey(..),Doorstate(..),Plate,Block,Door,Property,Ball,Line,Bricks,State(..),Dir(..),init_model1,model_init,getBrickPos,model_level1,model_level2,model_level3,initBricks,init_model2,init_model3)
import Color exposing (BallColor(..),NormalColor(..))
import Levels exposing (Level,initLevel1,initLevel2,initLevel3)
import Levels exposing (GMap,End)
import Levels exposing (initEnding1)
import Levels exposing (initEnding2)
import Levels exposing (initEnding3)
import Svg.Attributes exposing (operator)
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
    , door : Door
    }

type alias Door =
    { state : Doorstate
    , time : Float
    }

type Doorstate
    = Open
    | Closed

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
    | Begining
    | Ending
    
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
    Ball (250,700) ( x , -x ) (setProperty 0 100) initBallColor (setProperty 0 100) (setProperty 0 100) 

newBall : Ball -> Float -> Ball 
newBall ball x = 
    Ball (250,700) ( x , -x ) ball.energy ball.color ball.mp ball.exp

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
init_Map1 : Bricks 
init_Map1  = 
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
        lll = getline (275,100) (0,0) 1
        l = List.concat [l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11]
        color = List.concat[List.repeat 32 Grey,List.repeat 4 Purple,List.repeat 6 Yellow,[Noth]]
        lb = [3,21,30,1,5,10,14,19,23,28,32]
        ncolor = changeColor color lb 1
    in
        zip l ncolor

init_Map3 : Bricks
init_Map3 =    
    let     
        l1 = getline (25,75) (50,0) 11
        l2 = getline (25,125) (0,50) 4
        l22 = getline (75,325) (50,50) 5
        l222 = getline (25,325) (50,50) 6
        l3 = getline (525,125) (0,50) 4
        l33 = getline (475,325) (-50,50) 4
        l333 = getline (525,325) (-50,50) 5
        l4 = getline (225,125) (100,0) 2
        l5 = getline (225,175) (50,0) 3
        l6 = getline (125,125) (300,0) 2
        l7 = getline (75,125) (100,0) 2 
        l77 = getline (125,175) (0,0) 1
        l8 = getline (375,125) (100,0) 2
        l88 = getline (425,175) (0,0) 1
        l0 = getline (25,25) (100,0) 6
        la = getline (275,25) (0,0) 1
        l9 = getline (275,475) (0,0) 1
        lc = getline (225,425) (50,0) 3
        ld = getline (275,225) (0,50) 4
        l = List.concat [l1,l4,l5,l2,l3,l22,l222,l33,l333,l6,l7,l77,l8,l88,l0,l9,la,lc,ld]
        color = List.concat[ List.repeat 24 Black ,List.repeat 20 Grey,[Blue,Blue],List.repeat 6  Yellow,
                    List.repeat 2 Yellow,List.repeat 2 Blue,List.repeat 2 Yellow,[Blue,Blue],List.repeat 7 Purple
                    ,List.repeat 20 Grey]
        lb = [27,28,29,38,39]
        ncolor = changeColor color lb 1
    in
        zip l ncolor

init_Map2 : Bricks
init_Map2 =    
    let     
        l1 = getline (75,25) (100,0) 2
        l111 = getline (475,25) (-100,0) 2
        l2 = getline (25,75) (100,0) 6
        l22 = getline (125,125) (100,0) 4
        l3 = getline (75,175) (100,0) 5
        l33 = getline (25,175) (100,0) 2
        l333 = getline (525,175) (-100,0) 2
        l4 = getline (75,225) (100,0) 5
        l44 = getline (75,275) (100,0) 5
        l5 = getline (75,325) (100,0) 5
        l6 = getline (25,375) (100,0) 6
        l66 = getline (25,425) (100,0) 6
        l7 = getline (75,475) (100,0) 5
        l8 = getline (25,325) (500,0) 2
        l9 = getline (75,275) (100,0) 5
        l10 = getline (25,125) (500,0) 2
        l11 = getline (25,225) (500,0) 2
        l12 = getline (25,275) (500,0) 2
        l13 = getline (225,25) (100,0) 2
        l14 = getline (275,75) (0,0) 1
        l15 = getline (275,25) (0,0) 1
        l = List.concat[l1,l111,l2,l3,l33,l333,l4,l44,l5,l6,l66,l7,l8,l9,l22,l10,l11,l12,l13,l14,l15]
        color = List.concat [ [Yellow,Grey,Yellow,Grey] , List.repeat 42 Grey , [Black,Grey,Grey,Grey,Black] 
                            , [Yellow,Yellow] , List.repeat 5 Purple , List.repeat 4 Grey, [Blue,Blue], List.repeat 2 Yellow
                            , List.repeat 5 Black , [Noth]
                            ]
        --lb = [11,12,13,15,16,17,18,19,20,36,37,40,41] 
        lb = [11,12,14,15,16,17,18,19,35,36,39,40]
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
    Model 0 (1,1) (initBall 3) init_Map1 initPlate x False Begining 0 initLevel1 initEnding1 0 (Door Closed 0)

init_model1 : Model -> Model
init_model1 model = 
    Model 0 model.windowsize (initBall 3) init_Map1 initPlate 2 False Paused 0 initLevel1 initEnding1 0 (Door Closed 0)

init_model2 : Model -> Model
init_model2 model = 
    Model 0 model.windowsize (newBall model.ball 2) init_Map2 initPlate 2 False Paused 0 initLevel2 initEnding2 0 (Door Closed 0)

init_model3 : Model -> Model
init_model3 model =
    Model 0 model.windowsize (newBall model.ball 3) init_Map3 initPlate 2 False Paused 0 initLevel3 initEnding3 0 (Door Closed 0)

model_level1 : Model -> Model
model_level1 model = 
    { model | ball =  (newBall model.ball 3) , state = Paused } 

model_level2 : Model -> Model
model_level2 model = 
    { model | ball =  (newBall model.ball 2) , state = Paused } 

model_level3 : Model -> Model
model_level3 model =    
    { model | ball =  (newBall model.ball 3) , state = Paused } 





