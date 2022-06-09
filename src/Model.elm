module Model exposing (..)

type alias Model =
    { time : Float
    , windowsize : ( Float, Float )
    , ball : Ball
    , isDead : Bool
    , bricks : Bricks
    , plate : Plate
    , bounce : Bool
    , paused : Bool
    }

type alias Plate = 
    { state : Dir
    , pos : Float
    }

type alias Ball =
    { pos : (Float, Float)
    , vel : (Float, Float)
    }
type alias Line = 
    { p1 : (Float,Float)
    , p2 : (Float,Float)
    }



type alias Bricks = 
    List Block

type alias Block = 
    ( Float , Float )

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
    
initBall : Ball
initBall = 
    Ball (300, 565) ( 3 , -3 )

initBricks : Bricks
initBricks = 
    let
        rows = List.map (\x -> (toFloat x)) (List.map (\x ->  x*60+5) (List.range 0 9))
        cols = List.map (\x -> (toFloat x)) (List.map (\x ->  x*60+100) (List.range 0 4))
        line =
            \y -> List.map (\x -> Tuple.pair x y) rows
    in
        List.map line cols
            |> List.concat

initPlate : Plate
initPlate = Plate None 250


model_init : Model
model_init = 
    Model 0 ( 600, 600 ) initBall False initBricks initPlate False False





