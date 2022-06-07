module Model exposing (..)

type alias Model =
    { time : Float
    , windowsize : ( Float, Float )
    , ball : Ball
    , isDead : Bool
    , bricks : Bricks
    , plate : Int
    , bounce : Bool
    , paused : Bool
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

   
initBall : Ball
initBall = 
    Ball (500, 455) ( 1 , -1 )

initBricks : Bricks
initBricks = 
    let
        rows = List.map (\x -> (toFloat x)) (List.map (\x ->  x*100) (List.range 0 9))
        cols = List.map (\x -> (toFloat x)) (List.map (\x ->  x*21) (List.range 0 3))
        line =
            \y -> List.map (\x -> Tuple.pair x y) rows
    in
        List.map line cols
            |> List.concat

initPlate : Int
initPlate = 425

model_init : Model
model_init = 
    Model 0 ( 1000, 500 ) initBall False initBricks initPlate False False





