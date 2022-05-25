module Main exposing (..)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onResize)
import Debug exposing (toString)
import Debug exposing (toString)
import Html exposing (..)
import Html.Events exposing (keyCode)
import Html.Attributes as HtmlAttr exposing (..)
import Svg exposing (Svg, Attribute, svg, rect, defs, filter, feGaussianBlur, feMerge, feMergeNode)
import Svg.Attributes exposing (width, height, viewBox, x, y, rx, fill, id, stdDeviation, result)
import Task
import Json.Decode as Decode



type alias Game =
    { time : Float
    , windowsize : ( Float, Float )
    , ball : Ball
    , isDead : Bool
    , bricks : Bricks
    , plate : Plate
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

type alias Plate = ( Int , Int )

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

initPlate : Plate
initPlate = 400

init : Game
init =
    { time = 0
    , windowsize = ( 800, 600)
    , ball = initBall
    , isDead = False
    , pause = False
    , bounce = False
    , bricks = initBricks
    , plate = initPlate
    }

init : () -> ( Model, Cmd Msg )
init a =
    ( Game , Task.perform GetViewport getViewport )


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
                []
            ]
            [ viewBackground ]
                ++ viewBall game.ball
                ++ viewBlocks game.blocks
                ++ viewPlate game.plate
        ]


backgroundColor : Attribute Msg
backgroundColor =
    fill "white"

viewBackground : Svg Msg
viewBackground =
    rect [ x "0", y "0", width size, height size, backgroundColor ] []


viewBall : Ball -> Svg Msg
viewBall ball =
    ----

viewPlate : Plate -> Svg Msg
viewPlate plate = 
    ----

viewBlocks : List Block -> List (Svg Msg)
viewBlocks blocks =
    let
        ( strX, strY ) =
            ( toString block.x, toString block.y )
    in
        -- rect [ x strX, y strY, width "80", height "40", fill "purple", rx "0.2" ] []
        List map ...


-- Update


update : Msg -> Game -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick elapsed ->
            updateGame game

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
            ( { game | paused = not game.paused }, Cmd.none )

        ArrowPressed arrow ->
            ( move_plate arrow game, Cmd.none )


move_plate : ArrowKey -> Game -> Game
move_plate key game =
    let
        { plate } =
            game
        
        new_plate = 
            if key == LeftKey
                plate-10
            else if key == RightKey
                plate+10
            else   
                plate
    in
        { game | plate = new_plate }

updateGame : Game -> ( Game , Cmd Msg )
    if game.isDead || game.paused then
        ( game, Cmd.none)
    else
        ( game, Cmd.none)
            |> ballHitTheBrick
            |> ballAtTheEdge
            |> ballAtTheBottom
            |> updateBall
            |> updateBrick

ballHitTheBrick : ( Game , Cmd Msg ) -> ( Game, Cmd Msg)
ballHitTheBrick ( game, cmd) =
    let
        mball =
            game.ball
        
        posx = Tuple.first (mball.pos)
        posy = Tuple.second (mball.pos)
        vlx = Tuple.first (mball.vel)
        vly = Tuple.second (mball.vel)
        -- twist the velocity direction
        if posx == 0  || posx == 800 then 
            --nball-velocity-twist here
            nball = { mball | vel = }

    in
        ( { game | ball = nball }, cmd )


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
            Key_Up

        40 ->
            Key_Down

        37 ->
            Key_Left

        39 ->
            Key_Right

        _ ->
            Key_None