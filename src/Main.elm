module Main exposing (..)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onResize)
import Debug exposing (toString)
import Debug exposing (toString)
import Html exposing (..)
import Html.Events exposing (keyCode)
import Html.Attributes as HtmlAttr exposing (..)
import Task
import Svg exposing (Svg, Attribute, svg, rect, defs, filter, feGaussianBlur, feMerge, feMergeNode)
{-import Svg.Attributes exposing (width, height, viewBox, x, y, rx, fill, id, stdDeviation, result)-}
import Svg.Attributes as SvgAttr
import Json.Decode as Decode


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

type Msg
    = ArrowPressed ArrowKey
    | Tick Float
    | GetViewport Viewport
    | Resize Int Int


--Main


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



--Initialization



    
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

init : () -> ( Model, Cmd Msg )
init a =
    ( model_init , Task.perform GetViewport getViewport )





--View


getx : Float -> Float -> String
getx x xx = 
    String.fromFloat (x/xx*1518)

gety : Float -> Float -> String
gety y yy = 
    String.fromFloat (y/yy*759)
getr r =
    String.fromFloat (r/500*759)

drawreac : ( Float , Float ) -> ( Float , Float ) -> ( Float , Float ) -> String -> Svg Msg
drawreac (x,y) (dx,dy) (xx,yy) color = 
    Svg.rect [ SvgAttr.x (getx x xx), SvgAttr.y (gety y yy), SvgAttr.width (getx dx xx) , SvgAttr.height (gety dy yy) , SvgAttr.fill color ][ ]
{- (x,y) : the left up point , (dx,dy) : size of the reactangle , (xx,yy) : windows size -}

drawcir : ( Float , Float ) -> ( Float , Float ) -> Float -> String -> Svg Msg
drawcir (x,y) (xx,yy) r color =
    Svg.circle [ SvgAttr.cx (getx x xx), SvgAttr.cy (gety y yy), SvgAttr.r (getr r) , SvgAttr.fill color ][] 
{- (x,y) : center point , (xx,yy) : windows size -}

view : Model -> Html Msg
view model =
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "fixed"
        , HtmlAttr.style "left" "0"
        , HtmlAttr.style "top" "0"
        ]
        [ Svg.svg
            [ SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            (
                List.concat
                [ 
                    [viewPlate model model.plate],
                    viewBlocks model model.bricks,
                    [viewBall model model.ball]
                ]
            )
            {-[ viewBackground ]
                viewBackground
                ++ 
            -}
        ]




backgroundColor : Attribute Msg
backgroundColor =
    SvgAttr.fill "white"

viewPlate : Model -> Int -> Svg Msg
viewPlate model plate = 
    let
        (xx,yy) = model.windowsize
        x = model.plate
    in
        drawreac (toFloat x,yy-30) (150,10) (xx,yy) "#00CDCD"

viewBackground : Svg Msg
viewBackground =
    drawreac (0,0) (1000,500) (1000,500) "white"


drawBlocks : ( Float , Float ) -> ( Int , Int ) -> Svg Msg
drawBlocks windows ( x , y ) = 
    drawreac (toFloat x,toFloat y) (99,20) windows "#00CDCD"


viewBlocks : Model -> List Block -> List (Svg Msg)
viewBlocks model blocks =
    List.map (drawBlocks model.windowsize) blocks 


viewBall : Model -> Ball -> Svg Msg
viewBall model ball =
    drawcir ( toFloat (Tuple.first ball.pos) , toFloat (Tuple.second ball.pos) ) model.windowsize 15 "#FFEC8B"

{--}


-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model , Cmd.none )
{-
    case msg of
        Tick elapsed ->
            updateGame model

        GetViewport { viewport } ->
            ( { model
                | windowsize =
                    ( viewport.width
                    , viewport.height
                    )
              }
            , Cmd.none
            )

        Resize wid hei ->
            ( { model
                | windowsize =
                    ( toFloat wid
                    , toFloat hei
                    )
              }
            , Cmd.none
            )
        ArrowPressed Space ->
            ( { model | paused = not model.paused }, Cmd.none )

        ArrowPressed arrow ->
            ( move_plate arrow model, Cmd.none )
-}


move_plate : ArrowKey -> Model -> Model
move_plate keyy model =
    let
        old_plate =
            model.plate
        
        new_plate = 
            if keyy == LeftKey then
                old_plate-10
            else 
            if keyy == RightKey then
                old_plate+10
            else   
                old_plate
    in
        { model | plate = new_plate }

updateGame : Model -> ( Model , Cmd Msg )
updateGame model = 
    if model.isDead || model.paused then
        ( model, Cmd.none)
    else
        ( model, Cmd.none)
        {-    |> ballHitTheBrick
            |> ballAtTheEdge
            |> ballAtTheBottom
            |> updateBall
            |> updateBrick-}

ballHitTheBrick : ( Model , Cmd Msg ) -> ( Model, Cmd Msg)
ballHitTheBrick ( model, cmd) =
    let
        mball =
            model.ball
        
        posx = Tuple.first (mball.pos)
        posy = Tuple.second (mball.pos)
        vlx = Tuple.first (mball.vel)
        vly = Tuple.second (mball.vel)
        nball = mball
        -- twist the velocity direction
       {- if posx == 0  || posx == 800 then 
            --nball-velocity-twist here
            nball = mball -}
    in
        ( { model | ball = nball }, cmd )

{-
ballAtTheEdge : ( Game, Cmd Msg ) -> ( Game, Msg )
ballAtTheEdge ( game, cmd ) = 
    let 

    in

ballAtTheBottom : ( Game, Cmd Msg ) -> ( Game, Msg )
ballAtTheBottom ( game, cmd ) = 
    let 

    in



updateBrick : ( Game, Cmd Msg ) -> ( Game, Msg )
updateBrick ( game, cmd ) = 
    let 
    -- logic to edit the List Bricks in the game properties 

    in
-}

-- Subscription


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onAnimationFrameDelta Tick
        , onResize Resize
        , onKeyDown (Decode.map key keyCode)
        ]

key : Int -> Msg
key keycode =
    case keycode of
        38 ->
            ArrowPressed UpKey

        40 ->
            ArrowPressed DownKey

        37 ->
            ArrowPressed LeftKey

        39 ->
            ArrowPressed RightKey

        _ ->
            ArrowPressed NoKey