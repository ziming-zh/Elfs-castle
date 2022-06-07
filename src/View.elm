module View exposing (..)
import Svg exposing (Svg, Attribute, svg, rect, defs, filter, feGaussianBlur, feMerge, feMergeNode)
{-import Svg.Attributes exposing (width, height, viewBox, x, y, rx, fill, id, stdDeviation, result)-}
import Svg.Attributes as SvgAttr
import Message exposing (Msg(..))
import Model exposing (Model,Block,Ball)
import Html exposing (..)
import Html.Attributes as HtmlAttr exposing (..)

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




backgroundColor : Html.Attribute Msg
backgroundColor =
    SvgAttr.fill "white"

viewPlate : Model -> Int -> Svg Msg
viewPlate model plate = 
    let
        (xx,yy) = model.windowsize
        x = model.plate
    in
        drawreac (toFloat x,yy+(-30)) (150,10) (xx,yy) "#00CDCD"

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
