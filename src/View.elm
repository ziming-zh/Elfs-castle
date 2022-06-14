module View exposing (view)
import Svg exposing (Svg, Attribute, svg, rect, defs, filter, feGaussianBlur, feMerge, feMergeNode)
{-import Svg.Attributes exposing (width, height, viewBox, x, y, rx, fill, id, stdDeviation, result)-}
import Svg.Attributes as SvgAttr
import Message exposing (Msg(..))
import Model exposing (Model,Bricks,Ball,Plate,State,getBrickPos,initBricks)
import Html exposing (..)
import Html.Attributes as HtmlAttr exposing (..)
import Html.Events exposing (onClick)
import Color exposing (Color)
import Markdown
import View.Pacman as View
import Color exposing (BallColor(..),NormalColor(..),type2color)
import Levels exposing (Level,End, GMap,Condition)
import Levels exposing (initEnding1,initEnding2,initEnding3)
import Array exposing (Array)

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
                
                Model.Changing ->
                    ( "Resume" , Pause )
    in
    button
      [ style "background" "#B1E4F1"
        , style "border" "0"
        , style "bottom" "30px"
        , style "color" "#fff"
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "18px"
        , style "font-weight" "300"
        , style "height" "60px"
        , style "left" "30px"
        , style "line-height" "60px"
        , style "padding" "0"
        , style "position" "absolute"
        , style "width" "120px"
        , onClick msg
        ]
        [ text txt ]
        
renderNextButton : Model -> Html Msg
renderNextButton model =
    let
        ( txt, msg ) =
            ( "Next level" , Next )
    in
    button
      [ style "background" "#B1E4F1"
        , style "border" "0"
        , style "bottom" "100px"
        , style "color" "#fff"
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "10px"
        , style "font-weight" "300"
        , style "height" "30px"
        , style "left" "30px"
        , style "line-height" "30px"
        , style "padding" "0"
        , style "position" "absolute"
        , style "width" "120px"
        , style "displaying" 
            (if model.state == Model.Changing then "block"
            else "none")
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

renderTip : Model -> String -> Int -> Int -> String -> Html Msg
renderTip model str x y color =
    div 
        [ style "height" "50px"
        , style "left" ((String.fromInt x)++"px")
        , style "position" "relative"
        , style "top" ("-"++(String.fromInt y)++"px")
        , style "display" 
            (if model.ball.mp.val >= model.ball.mp.max then "block"
            else "none")
        ]
        [ renderTxT str color]

checkstatecolor : Model -> String
checkstatecolor model= 
    
        case model.ball.color of 
            Red x -> type2color (Red x)
            _ -> "#33FFFF"

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
            , drawreac (1,321) (model.ball.mp.val*2,8) (checkstatecolor model)
            ]
        ]

renderNeed : Model -> Html Msg
renderNeed model =
    let
        (x,y,z) = model.level.pass
    in
    div
        [ style "bottom" "80px"
        , style "color" "#34495f"
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "5px"
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
            , drawreac (10,115) (50,50) "#33CCFF"
            , drawreac (10,180) (50,50) "#3380BC"
            ]
            ,renderX "X" 70 210 "#bdc3c7",renderX "X" 70 225 "#bdc3c7",renderX "X" 70 240 "#bdc3c7"
            ,renderX (String.fromInt (Basics.max 0 x)) 100 450 "#242424",renderX (String.fromInt (Basics.max 0 y)) 100 465 "#242424"
            ,renderX (String.fromInt (Basics.max 0 z)) 100 480 "#242424"
            ,renderX "mp:" -145 440 "#bdc3c7",renderTip model "Press F!" -140 440 "#242424" 
        ]

renderPanel : Model -> Html Msg
renderPanel model =
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
        [ renderTitle "Elf"
        , renderTxT "Money" "#bdc3c7"
        , renderCount  model.score
       -- , renderCountt  (Tuple.first model.ending.pos)
     --   , renderCountt  (Tuple.second model.ending.pos)
      --  , renderCountt  model.dt
        , renderEnd model.state "You lose! Please Try Again!"
        , renderGameButton model.state
        , renderNextButton model
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
        , style "width" "580px"
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
                
                ([renderNeed model
                ,renderinfor model
                ,renderPanel model
                ,renderBackground 
                ,renderGame model
                ,renderHouse model       
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
                            Black -> ( map.color , pass )
                            Nocolor -> ( map.color , pass )
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
                    _ -> ( map.color , pass )
                    
        in  
            resmap ( { map | color = nmap } ) npass (i+1)

initHouse : End -> Condition -> Bricks
initHouse end pass = 
    let
        k = 1+(Tuple.first end.pos)/800
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
        -- zip sol (List.map (\y -> (revealColor map.color (Tuple.first y) (Tuple.second y))) (colorChecker condition))
       -- revealColor Purple pur map.color
       -- |> revealColor Yellow yel
       -- |> revealColor Blue blue
        nmap.color 
        |> zip sol


renderHouse : Model -> Html Msg
renderHouse model = 
    let
        id = model.level.id
        pass = model.level.pass
        k = 1+(Tuple.first model.ending.pos)/800
    in
    
    div
        [ 
          style "color" "#34495f"
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "5px"
        , style "left" ((String.fromFloat (( Tuple.first model.ending.pos )-600))++"px")
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
                1 -> (viewcBlocks (1+(Tuple.first model.ending.pos)/800) (initHouse model.ending pass))
                2 -> (viewcBlocks (1+(Tuple.first model.ending.pos)/800) (initHouse model.ending pass))
                3 -> (viewcBlocks (1+(Tuple.first model.ending.pos)/800) (initHouse model.ending pass))
                _ -> (viewcBlocks (1+(Tuple.first model.ending.pos)/800) (initHouse model.ending pass))
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
    drawreac (x, y) (50,50)  (type2color (Normal normalcolor))


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