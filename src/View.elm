module View exposing (..)
import Svg exposing (Svg, Attribute, svg, rect, defs, filter, feGaussianBlur, feMerge, feMergeNode)
{-import Svg.Attributes exposing (width, height, viewBox, x, y, rx, fill, id, stdDeviation, result)-}
import Svg.Attributes as SvgAttr
import Message exposing (Msg(..))
import Model exposing (Model,Bricks,Ball,Plate,State)
import Html exposing (..)
import Html.Attributes as HtmlAttr exposing (..)
import Html.Events exposing (onClick)
import Color exposing (Color)
import Markdown
import View.Pacman as View
import Model exposing (getBrickPos)
import Color exposing (BallColor(..),NormalColor(..),type2color)
import Levels exposing (Level)

getx : Float -> String
getx x  = 
    String.fromFloat (x)

gety : Float -> String
gety y = 
    String.fromFloat (y)
getr r =
    String.fromFloat (r)

renderGameButton : State -> Html Msg
renderGameButton state =
    let
        ( txt, msg ) =
            case state of
                Model.GG ->
                    ( "New game", Start )

                Model.Playing ->
                    ( "Pause", Pause )

                Model.Paused ->
                    ( "Resume", Resume ) 
    in
    button
        [ style "background" "#B1E4F1"
        , style "border" "0"
        , style "bottom" "30px"
        , style "color" "#fff"
        , style "cursor" "pointer"
        , style "display" "block"
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "18px"
        , style "font-weight" "300"
        , style "height" "60px"
        , style "left" "30px"
        , style "line-height" "60px"
        , style "outline" "none"
        , style "padding" "0"
        , style "position" "absolute"
        , style "width" "120px"
        , onClick msg
        ]
        [ text txt ]

renderX : String -> Int -> Int -> String -> Html Msg
renderX str x y color =
    div 
        [ style "height" "50px"
        , style "left" ((String.fromInt x)++"px")
        , style "position" "relative"
        , style "top" ("-"++(String.fromInt y)++"px")
        ]
        [ renderTxT str color]

renderinfor : Model -> Html Msg
renderinfor model = 
    div
        [ style "bottom" "80px"
        , style "color" "#34495f"
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "14px"
        , style "left" "-220px"
        , style "position" "absolute"
        , style "right" "0"
        , style "top" "0"
        ]
        [ Svg.svg
            [ SvgAttr.width (getx 300)
            , SvgAttr.height (gety 700)
            ]
            [ drawreac (0,320) (202,1) "#242424" , drawreac (0,320) (1,10) "#242424" , drawreac (0,330) (202,1) "#242424" , drawreac (202,320) (1,10) "#242424"
            , drawreac (1,321) (model.ball.mp.val*2,8) "#33FFFF"
            ]
        ]

renderNeed : Level -> Html Msg
renderNeed { pass } =
    let
        (x,y,z) = pass
    in
    div
        [ style "bottom" "80px"
        , style "color" "#34495f"
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "14px"
        , style "left" "-150px"
        , style "position" "absolute"
        , style "right" "0"
        , style "top" "0"
        ]
        [ Svg.svg
            [ SvgAttr.width (getx 100)
            , SvgAttr.height (gety 230)
            ]
            [ drawreac (10,50) (50,50) "#FFB266"
            , drawreac (10,115) (50,50) "#3399FF"
            , drawreac (10,180) (50,50) "#FF66B2"
            ]
            ,renderX "X" 70 210 "#bdc3c7",renderX "X" 70 225 "#bdc3c7",renderX "X" 70 240 "#bdc3c7"
            ,renderX (String.fromInt (Basics.max 0 x)) 100 450 "242424",renderX (String.fromInt (Basics.max 0 y)) 100 465 "242424"
            ,renderX (String.fromInt (Basics.max 0 z)) 100 480 "242424"
        ]

renderPanel : Model -> Html Msg
renderPanel model =
    div
        [ style "bottom" "80px"
        , style "color" "#34495f"
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "14px"
        , style "left" "600px"
        , style "padding" "0 30px"
        , style "position" "absolute"
        , style "right" "0"
        , style "top" "0"
        ]
        [ renderTitle "Elf"
        , renderTxT "Score" "#bdc3c7"
        , renderCount  model.score
        --, renderCountt  model.ball.mp.val
        --, renderCountt  model.ball.mp.max
        , renderEnd model.state "The Elf is Upset! Please Try Again!"
        , renderGameButton model.state
        ]



renderTitle : String -> Html Msg
renderTitle txt =
    div
        [ style "color" "#34495f"
        , style "font-size" "40px"
        , style "line-height" "60px"
        , style "margin" "30px 0 0"
        ]
        [ text txt ]


renderTxT : String -> String -> Html Msg
renderTxT txt color =
    div
        [ style "color" color
        , style "font-weight" "700"
        , style "font-size" "40px"
        , style "line-height" "1"
        , style "margin" "30px 0 0"
        ]
        [ text txt ]

renderEnd : State -> String -> Html Msg
renderEnd state txt =
    div
        [ style "font-size" "40px"
        , style "font-weight" "700"
        , style "line-height" "1"
        , style "margin" "30px 0 0"
        , style "bottom" "250px"
        , style "left" "30px"
        , style "position" "absolute"
        , style "display"
          ( if state == Model.GG then "block"
            else "none"
          )
        ]
        [ text txt ]



renderCount : Int -> Html Msg
renderCount n =
    div
        [ style "color" "#3993d0"
        , style "font-size" "30px"
        , style "line-height" "1"
        , style "margin" "5px 0 0"
        ]
        [ text (String.fromInt n) ]

renderCountt : Float -> Html Msg
renderCountt n =
    div
        [ style "color" "#3993d0"
        , style "font-size" "30px"
        , style "line-height" "1"
        , style "margin" "5px 0 0"
        ]
        [ text (String.fromFloat n) ]


drawreac : ( Float , Float )  -> ( Float , Float ) -> String -> Html Msg
drawreac (x,y) (dx,dy) color = 
    Svg.rect 
    [ SvgAttr.x (getx x )
    , SvgAttr.y (gety y )
    , SvgAttr.width (getx dx ) 
    , SvgAttr.height (gety dy ) 
    , SvgAttr.fill color 
    , SvgAttr.stroke color
    , SvgAttr.strokeWidth "0.5"
    ]
    []
    
{- (x,y) : the left up point , (dx,dy) : size of the reactangle , (xx,yy) : windows size -}




renderBackground : Html Msg
renderBackground =
    div
        [ style "background" "rgba(236, 240, 241, 0.15)"
        , style "color" "#34495f"
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "18px"
        , style "height" "600px"
        , style "left" "0"
        , style "line-height" "1.5"
        , style "padding" "0 15px"
        , style "position" "absolute"
        , style "top" "0"
        , style "width" "800px"
       -- , style "display" "block"
        ]
        [ 
        ]


pixelWidth : Float
pixelWidth =
    600


pixelHeight : Float
pixelHeight =
    800

view : Model -> Html Msg
view model =
    let
        ( w, h ) =
            model.windowsize

        r =
            if w / h > (pixelWidth+200) / pixelHeight then
                Basics.min 1 (h / pixelHeight)

            else
                Basics.min 1 (w / (pixelWidth+200))
    in
        div
            [ HtmlAttr.style "width" "100%"
            , HtmlAttr.style "height" "100%"
            , HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "left" "0"
            , HtmlAttr.style "top" "0"
            ]
            [ div
                [ HtmlAttr.style "width" (String.fromFloat pixelWidth ++ "px")
                , HtmlAttr.style "height"  (String.fromFloat pixelHeight ++ "px")
                , HtmlAttr.style "position" "absolute"
                , HtmlAttr.style "left" (String.fromFloat ((w - pixelWidth * r) / 2) ++ "px")
                , HtmlAttr.style "top" (String.fromFloat ((h - pixelHeight * r) / 2) ++ "px")
                , HtmlAttr.style "transform-origin" "0 0"
                , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat r ++ ")")
                ]
                
                ([renderNeed model.level
                ,renderinfor model
                ,renderPanel model
                ,renderBackground 
                ,renderGame model       
                ])
                    
                
            ]
           
renderGame : Model -> Html Msg
renderGame model = 
    Svg.svg
    [ SvgAttr.width (getx 600.0)
    , SvgAttr.height (gety 800.0) 
    ]     
    ([viewPlate model model.plate
        ,viewBall model model.ball]
        ++
        (viewBlocks model.bricks))



backgroundColor : Html.Attribute Msg
backgroundColor =
    SvgAttr.fill "white"

viewPlate : Model -> Plate -> Html Msg
viewPlate model plate = 
    let
       
        x = model.plate.pos
    in
        drawreac (x,780) (150,10)  "#00CDCD"



drawBlocks : ((Float,Float) , NormalColor) -> Html Msg
drawBlocks  ((x,y),normalcolor) = 
    drawreac (x, y) (50,50)  (type2color (Normal normalcolor))


viewBlocks : Bricks -> List (Html Msg)
viewBlocks blocks =
    List.map drawBlocks blocks


drawcir : ( Float , Float ) -> Float -> String -> Html Msg
drawcir (x,y)  r color =
    
    Svg.ellipse [ SvgAttr.cx (getx x ), SvgAttr.cy (gety y ), SvgAttr.rx (getx r) , SvgAttr.ry (gety r ) ,  SvgAttr.fill color ][] 
    
   {- Svg.circle [ SvgAttr.cx (getx x xx), SvgAttr.cy (gety y yy), SvgAttr.r (getr r yy) , SvgAttr.fill color ][] -}
{- (x,y) : center point , (xx,yy) : windows size -}
viewBall : Model -> Ball -> Html Msg
viewBall model ball =
    -- drawcir ( Tuple.first ball.pos , Tuple.second ball.pos ) 15 "#FFEC8B"
    -- View.pacman model
    let
        (vx,vy) =ball.vel
        neg num = 
            if num > 0 then 1
            else 0
        
        percentage = 
            if round (toFloat(round model.time)/2) == round (toFloat(round (model.time-1))/2)
                then 0.75
            else 0.9
        offset = atan (vy/vx) / (2*pi)+  1/2*(neg vx)+ (1-percentage)/2 +(-1/4)
       
        fanshapes = View.FanShape (offset*100) (percentage*100) (type2color ball.color) ball.pos
    in
       View.viewFanShape fanshapes