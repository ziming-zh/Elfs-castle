module View exposing (..)
import Svg exposing (Svg, Attribute, svg, rect, defs, filter, feGaussianBlur, feMerge, feMergeNode)
{-import Svg.Attributes exposing (width, height, viewBox, x, y, rx, fill, id, stdDeviation, result)-}
import Svg.Attributes as SvgAttr
import Message exposing (Msg(..))
import Model exposing (Model,Block,Ball,Plate)
import Html exposing (..)
import Html.Attributes as HtmlAttr exposing (..)

getx : Float -> Float -> String
getx x xx = 
    String.fromFloat (x/1000*xx)

gety : Float -> Float -> String
gety y yy = 
    String.fromFloat (y/500*yy)
getr r yy =
    String.fromFloat (r/500*yy)

drawreac : ( Float , Float ) -> ( Float , Float ) -> ( Float , Float ) -> String -> Svg Msg
drawreac (x,y) (dx,dy) (xx,yy) color = 
    Svg.rect [ SvgAttr.x (getx x xx), SvgAttr.y (gety y yy), SvgAttr.width (getx dx xx) , SvgAttr.height (gety dy yy) , SvgAttr.fill color ][ ]
{- (x,y) : the left up point , (dx,dy) : size of the reactangle , (xx,yy) : windows size -}


view : Model -> Html Msg
view model =
    div
        [ HtmlAttr.style "width" "100%"
        , HtmlAttr.style "height" "100%"
        , HtmlAttr.style "position" "fixed"
        , HtmlAttr.style "left" "0"
        , HtmlAttr.style "top" "0"
        ]
        [ 
        Svg.svg
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




backgroundColor : Html.Attribute Msg
backgroundColor =
    SvgAttr.fill "white"

viewPlate : Model -> Plate -> Svg Msg
viewPlate model plate = 
    let
        (xx,yy) = model.windowsize
        x = model.plate.pos
    in
        drawreac (x,470) (150,10) (xx,yy) "#00CDCD"

viewBackground : Svg Msg
viewBackground =
    drawreac (0,0) (1000,500) (1000,500) "white"


drawBlocks : ( Float , Float ) -> ( Float , Float ) -> Svg Msg
drawBlocks windows ( x , y ) = 
    drawreac (x, y) (99,20) windows "#00CDCD"


viewBlocks : Model -> List Block -> List (Svg Msg)
viewBlocks model blocks =
    List.map (drawBlocks model.windowsize) blocks 


drawcir : ( Float , Float ) -> ( Float , Float ) -> Float -> String -> Svg Msg
drawcir (x,y) (xx,yy) r color =
    Svg.ellipse [ SvgAttr.cx (getx x xx), SvgAttr.cy (gety y yy), SvgAttr.rx (getx r xx) , SvgAttr.ry (gety r yy) ,  SvgAttr.fill color ][] 
    
   {- Svg.circle [ SvgAttr.cx (getx x xx), SvgAttr.cy (gety y yy), SvgAttr.r (getr r yy) , SvgAttr.fill color ][] -}
{- (x,y) : center point , (xx,yy) : windows size -}
viewBall : Model -> Ball -> Svg Msg
viewBall model ball =
    drawcir ( Tuple.first ball.pos , Tuple.second ball.pos ) model.windowsize 15 "#FFEC8B"
