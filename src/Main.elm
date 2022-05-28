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
    Ball (400, 600) (-40 , 30)

initBricks : Bricks
initBricks = 
    let
        rows = List.map (\x -> 40 + x*40) (List.range 0 5)
        cols = List.map (\x ->  x*80) (List.range 0 9)
        line =
            \y -> List.map (\x -> Tuple.pair x y) rows
    in
    List.map line cols
        |> List.concat

initPlate : Int
initPlate = 400

model_init : Model
model_init = 
    Model 0 ( 800, 600 ) initBall False initBricks initPlate False False

init : () -> ( Model, Cmd Msg )
init a =
    ( model_init , Task.perform GetViewport getViewport )

--View


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
            [ Svg.circle
                [ SvgAttr.cx (toString ((Tuple.first model.windowsize) / 2 + 100 * cos (model.time / 200)) ++ "px")
                , SvgAttr.cy (toString ((Tuple.second model.windowsize) / 2 + 100 * sin (model.time / 200)) ++ "px")
                , SvgAttr.r "20px"
                , SvgAttr.fill "black"
                ]
                [] ,
                viewBackground
            ]
            {-[ viewBackground ]
                ++ viewBall game.ball
                ++ viewBlocks game.blocks
                ++ viewPlate game.plate-}
        ]



viewBackground : Svg Msg
viewBackground =
    Svg.rect [ SvgAttr.x "0", SvgAttr.y "0", SvgAttr.width "100" , SvgAttr.height "100" , backgroundColor ] []

backgroundColor : Attribute Msg
backgroundColor =
    SvgAttr.fill "white"
{-
viewBall : Ball -> Svg Msg
viewBall ball =
    Svg.svg [][]

viewPlate : Plate -> Svg Msg
viewPlate plate = 
    Svg.svg [][]
-}
{-
viewBlocks : List Block -> List (Svg Msg)
viewBlocks blocks =
    let
        ( strX, strY ) =
            ( toString block.x, toString block.y )
    in
        -- rect [ x strX, y strY, width "80", height "40", fill "purple", rx "0.2" ] []
        List map ... -}


-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
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