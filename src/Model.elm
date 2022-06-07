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
    { pos : (Int, Int)
    , vel : (Int, Int)
    }


type alias Bricks = 
    List Block

type alias Block = 
    ( Int , Int )

type ArrowKey
    = NoKey
    | Space
    | LeftKey
    | RightKey
    | UpKey
    | DownKey

   
initBall : Ball
initBall = 
    Ball (500, 455) (-40 , 30)

initBricks : Bricks
initBricks = 
    let
        rows = List.map (\x ->  x*100) (List.range 0 9)
        cols = List.map (\x ->  x*21) (List.range 0 3)
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





