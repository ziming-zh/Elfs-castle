module View exposing (view)
import Svg exposing (Svg, Attribute, svg, rect, defs, filter, feGaussianBlur, feMerge, feMergeNode)
{-import Svg.Attributes exposing (width, height, viewBox, x, y, rx, fill, id, stdDeviation, result)-}
import Svg.Attributes as SvgAttr
import Message exposing (Msg(..))
import Model exposing (Model,Doorstate,Door,Bricks,Ball,Plate,State)
import Html exposing (..)
import Html.Attributes as HtmlAttr exposing (..)
import Html.Events exposing (onClick)
import Color exposing (Color)
import View.Pacman as View
import Color exposing (BallColor(..),NormalColor(..),type2color)
import Levels exposing (Level,End, GMap,Condition)
import Array exposing (Array)
import Model exposing (State(..))

import View.Interface exposing (..)




checkstatecolor : Model -> String
checkstatecolor model= 
    
        case model.ball.color of 
            Red x -> type2color (Red x)
            _ -> "#33FFFF"


life : Int -> String
life num = 
    if num == 0 then
        ""
    else
        "❤" ++ ( life (num-1) )

viewlives : Int -> Html Msg
viewlives lifes = 
    let
        str = life lifes
    in
    div
        [ style "color" "##8E9295"
        , style "font-weight" "300"
        , style "font-size" "25px"
        , style "line-height" "1"
        , style "margin" "30px 0 0"
        , style "left" "30px"
        , style "position" "absolute"
        , style "top" "500px"
        ]
        [   text "lives" ,
            div
            [ style "color" "#FF0000"
            , style "font-size" "24px"
            ]
            [ text str ]
        ]

renderPanel : Model -> Html Msg
renderPanel model =
    let
        (x,y) = model.ending.pos
        a = 1.0*13/180000
        b = -13/30
        t0 = (-b-Basics.sqrt(b*b-4*a*x))/(2*a)
        v0 = 13/30-650/1500/3000*t0
        v1 = v0-model.dt*650/1500/3000
        dx = (v0+v1)*model.dt/2
        (nx,ny) = 
            if x <= 650 then
                ( x + dx , y - dx/2 )
            else (x,y)
    in
    div
        [ style "bottom" "80px"
        , style "color" "#34495f"
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "4px"
        , style "left" "600px"
        , style "padding" "0 30px"
        , style "position" "absolute"
        , style "top" "0"
        ]
        [ renderTitle model
        , renderTxT "Money" "#bdc3c7"
        , renderCount  model.score
        , viewlives (1+model.live)
     --   , renderCountt  ( Basics.sqrt(b*b-4*a*x)-b )
     --   , renderCountt  model.dt
        , renderEnd model.state "You lose! Please Try Again!"
        , renderGameButton model.state
        , renderNextButton model
         ]



renderTitle : Model -> Html Msg
renderTitle model =
    div
        [ style "color" "#34495f"
        , style "font-size" "40px"
        , style "line-height" "60px"
        , style "margin" "30px 0 0"
        , style "width" "200%"
        ]
        [ text 
        ( case model.level.id of
            1 -> "Level1: Tower"
            2 -> "Level2: Turret"
            _ -> "Level3: Palace"
        )
        ]




renderEnd : State -> String -> Html Msg
renderEnd state txt =
    div
        [ style "font-size" "25px"
        , style "font-weight" "700"
        , style "line-height" "1"
        , style "bottom" "200px"
        , style "left" "-450px"
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



    
{- (x,y) : the left up point , (dx,dy) : size of the reactangle , (xx,yy) : windows size -}




renderBackground : Model -> Html Msg
renderBackground model =
    let
        k = 1-(Tuple.first model.ending.pos)/650
    in
    div
        [ style "background" "rgba(236, 240, 241, 0.3)"
        , style "color" "#34495f"
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "18px"
        , style "height" "800px"
        , style "left" "0"
        , style "line-height" "1.5"
        , style "padding" "0 15px"
        , style "position" "absolute"
        , style "top" "0"
        , style "width" "600px"
        , style "opacity" (String.fromFloat k)
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

renderChanging : Model -> Html Msg
renderChanging model = 
    let
        k = (Tuple.first model.ending.pos)/650
    in
        div
        [ style "height" "50px"
        , style "left" "150px"
        , style "position" "relative"
        , style "top" "-700px"
        , style "opacity" (String.fromFloat k)
        , style "color" "#34495f"
        , style "font-size" "40px"
        , style "line-height" "60px"
        ]
        [ text 
            (case model.level.id of
                1 -> "Hooray! A Tower!!!"
                2 -> "Hooray! A Turret!!!"
                _ -> "Hooray! A Palace!!!"
            ) ]
renderGame : Model -> Html Msg
renderGame model = 
    let        
        k = 1-(Tuple.first model.ending.pos)/650
    in
    
    div
    [ HtmlAttr.style "opacity" (String.fromFloat k)
    ]
    [
    Svg.svg
    [ SvgAttr.width (getx 600.0)
    , SvgAttr.height (gety 800.0) 
    ]     
    ([viewPlate model model.plate
        ,viewBall model model.ball]
        ++
        (viewBlocks model.bricks))
    ]
zip : List a -> List b -> List (a, b)
zip xs ys =
  List.map2 Tuple.pair xs ys

changeColor : (NormalColor,Bool) -> NormalColor -> ( NormalColor, Bool)
changeColor (element,deleted) aimcolor  = 
    if deleted == True && element == aimcolor then
        (Nocolor, False)
    else 
        (element, False)

revealColor :  NormalColor -> Int -> List NormalColor -> List NormalColor
revealColor  color remaining map = 
    if remaining<=0 then
        map
    else 
        revealColor color (remaining+(-1)) (Tuple.first (List.unzip (List.map (\x -> changeColor (x,True) color ) map)))

resmap : GMap -> Condition -> Int -> GMap
resmap map pass i = 
    if i == (List.length map.color) + 1 then map
    else
        let 
            (y,b,p) = pass 
            color = List.head ( List.drop (i-1) map.color )
            ( nmap , npass ) = 
                case color of 
                    Just a -> 
                        case a of 
                            Yellow -> 
                                if y > 0 then
                                    ( List.concat [ List.take (i-1) map.color , [Nocolor] , List.drop i map.color ] , (y-1,b,p) )
                                else ( map.color , pass )
                            Blue ->
                                if b > 0 then
                                    ( List.concat [ List.take (i-1) map.color , [Nocolor] , List.drop i map.color ] , (y,b-1,p) )
                                else ( map.color , pass )
                            Purple ->
                                if p > 0 then
                                    ( List.concat [ List.take (i-1) map.color , [Nocolor] , List.drop i map.color ] , (y,b,p-1) )
                                else ( map.color , pass )
                            Black -> ( map.color , pass )
                            _ -> ( map.color , pass )
                    _ -> ( map.color , pass )
                    
        in  
            resmap ( { map | color = nmap } ) npass (i+1)

initHouse : End -> Condition -> Bricks
initHouse end pass = 
    let
        k = 1+(Tuple.first end.pos)/650
        sizex = Tuple.first end.map.size
        sizey = Tuple.second end.map.size
        rows =  (List.map (\x ->  x*(k*60)+5) ( List.map Basics.toFloat (List.range (round (5- (toFloat sizex)/2)) (round (4+ (toFloat sizex)/2)))) )
        cols = (List.map (\x ->  x*(k*60)+50) ( List.map Basics.toFloat (List.range 0 sizey)) )
        line =
            \y -> List.map (\x -> Tuple.pair x y) rows
        sol =List.map line cols
            |> List.concat
        yel = case pass of 
            (a,_,_) -> a
        blue = case pass of 
            (_,b,_) -> b
        pur = case pass of 
            (_,_,c) -> c
        nmap = resmap end.map pass 1
    in
        nmap.color 
        |> zip sol


renderHouse : Model -> Html Msg
renderHouse model = 
    let
        id = model.level.id
        pass = model.level.pass
        k = 1+(Tuple.first model.ending.pos)/650
    in
    
    div
        [ 
          style "color" "#34495f"
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "5px"
        , style "left" ((String.fromFloat (( Tuple.first model.ending.pos )-700))++"px")
        , style "position" "absolute"
        , style "top" ((String.fromFloat (( Tuple.second model.ending.pos )+1100))++"px")
        , style "zoom" "0.5" 
        ]
        [
        Svg.svg
        [ SvgAttr.width (getx (600.0*k))
        , SvgAttr.height (gety (500.0*k)) 
        ]     
        (List.concat 
            [ [drawreac (0,0) (600.0*k,500.0*k) "#FFFFFF"],
            case id of
                1 -> (viewcBlocks (1+(Tuple.first model.ending.pos)/650) (initHouse model.ending pass))
                2 -> (viewcBlocks (1+(Tuple.first model.ending.pos)/650) (initHouse model.ending pass))
                3 -> (viewcBlocks (1+(Tuple.first model.ending.pos)/650) (initHouse model.ending pass))
                _ -> (viewcBlocks (1+(Tuple.first model.ending.pos)/650) (initHouse model.ending pass))
            ]
        )
        ]
    



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
    drawreac (x+10, y+10) (40,40)  (type2color (Normal normalcolor))


viewBlocks : Bricks -> List (Html Msg)
viewBlocks blocks =
    List.map drawBlocks blocks

drawcBlocks : Float -> ((Float,Float) , NormalColor) -> Html Msg
drawcBlocks k ((x,y),normalcolor) = 
    drawreac (x, y) (k*50,k*50)  (type2color (Normal normalcolor))


viewcBlocks : Float -> Bricks -> List (Html Msg)
viewcBlocks k blocks =
    List.map (drawcBlocks k) blocks

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
        (vx,vy) = ball.vel
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




viewdoor : Model -> Html Msg
viewdoor model = 
    let
        ( x , y ) = 
            case model.level.id of 
                1 -> ( 283 , 108 )
                2 -> ( 283 , 33 )
                _ -> ( 283 , 133 )
        k = Basics.max 0 (1-(Tuple.first model.ending.pos)/650)
        t = model.door.time
        k1 = Basics.max 0 (1-t/1000)
        k2 = Basics.min 1 (t/1000)
        img1 = "./assets/closed.png"
        img2 = "./assets/open.png"
    in
    div
    [HtmlAttr.style "opacity" (String.fromFloat k)]
    [ Html.img
        [ HtmlAttr.src img1
        , style "width" "45px"
        , style "height" "45px"
        , style "position" "absolute"
        , style "left" ((String.fromInt x)++"px")
        , style "top" ((String.fromInt y)++"px")
        , HtmlAttr.style "opacity" (String.fromFloat k1)
        ] []
    ,Html.img
        [ HtmlAttr.src img2
        , style "width" "45px"
        , style "height" "45px"
        , style "position" "absolute"
        , style "left" ((String.fromInt x)++"px")
        , style "top" ((String.fromInt y)++"px")
        , HtmlAttr.style "opacity" (String.fromFloat k2)
        ] []
    ]



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
            (
            [
            if model.state == Begining then
                viewBegining model (w,h)
            else if model.state == Ending then
                viewEnding model (w,h)
            else
                div
                    [ HtmlAttr.style "width" (String.fromFloat pixelWidth ++ "px")
                    , HtmlAttr.style "height"  (String.fromFloat pixelHeight ++ "px")
                    , HtmlAttr.style "position" "absolute"
                    , HtmlAttr.style "left" (String.fromFloat ((w - pixelWidth * r) / 2) ++ "px")
                    , HtmlAttr.style "top" (String.fromFloat ((h - pixelHeight * r) / 2) ++ "px")
                    , HtmlAttr.style "transform-origin" "0 0"
                    , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat r ++ ")")
                    ]
                    
                    ([renderNeed model
                    ,renderinfor model
                    ,renderPanel model
                    ,renderBackground model
                    ,renderGame model
                    ,renderHouse model
                    ,renderChanging model
                    ,viewdoor model
                    ])
            ])
