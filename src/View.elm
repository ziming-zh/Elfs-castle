module View exposing (..)
import Svg exposing (Svg, Attribute, svg, rect, defs, filter, feGaussianBlur, feMerge, feMergeNode)
{-import Svg.Attributes exposing (width, height, viewBox, x, y, rx, fill, id, stdDeviation, result)-}
import Svg.Attributes as SvgAttr
import Message exposing (Msg(..))
import Model exposing (Model,Block,Ball,Plate)
import Html exposing (..)
import Html.Attributes as HtmlAttr exposing (..)
import Color exposing (Color)
import Markdown
getx : Float -> String
getx x  = 
    String.fromFloat (x)

gety : Float -> String
gety y = 
    String.fromFloat (y)
getr r =
    String.fromFloat (r)


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
        [ renderTitle "Tetris"
        , renderLabel "Score"
        , renderCount  3252
        , renderLabel "Lines Cleared"
        , renderLabel "Next Shape"
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


renderLabel : String -> Html Msg
renderLabel txt =
    div
        [ style "color" "#bdc3c7"
        , style "font-weight" "300"
        , style "line-height" "1"
        , style "margin" "30px 0 0"
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



renderControlButton : String -> List (Html.Attribute Msg) -> Html Msg
renderControlButton txt attrs =
    div
        ([ style "background" "#ecf0f1"
         , style "border" "0"
         , style "color" "#34495f"
         , style "cursor" "pointer"
         , style "text-align" "center"
         , style "-webkit-user-select" "none"
         , style "display" "block"
         , style "float" "left"
         , style "font-family" "Helvetica, Arial, sans-serif"
         , style "font-size" "24px"
         , style "font-weight" "300"
         , style "height" "60px"
         , style "line-height" "60px"
         , style "margin" "20px 20px 0 0"
         , style "outline" "none"
         , style "padding" "0"
         , style "width" "60px"
         ]
            ++ attrs
        )
        [ text txt ]

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
        , style "display"
        
                "block"
        ]
        [ 
        ]


pixelWidth : Float
pixelWidth =
    400


pixelHeight : Float
pixelHeight =
    400

view : Model -> Html Msg
view model =
    let
        ( w, h ) =
            model.windowsize

        r =
            if w / h > pixelWidth / pixelHeight then
                Basics.min 1 (h / pixelHeight)

            else
                Basics.min 1 (w / pixelWidth)
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
                , HtmlAttr.style "height" (String.fromFloat pixelHeight ++ "px")
                , HtmlAttr.style "position" "flex-shrink"
                , HtmlAttr.style "left" (String.fromFloat ((w - pixelWidth * r) / 2) ++ "px")
                , HtmlAttr.style "top" (String.fromFloat ((h - pixelHeight * r) / 2) ++ "px")
                , HtmlAttr.style "transform-origin" "0 0"
                , HtmlAttr.style "transform" ("scale(" ++ String.fromFloat r ++ ")")
                ]
                
                ([renderPanel model
                ,renderBackground 
                ,renderGame model       
                ])
                    
                
            ]
           
renderGame : Model -> Html Msg
renderGame model = 
    Svg.svg
    [ SvgAttr.width (getx 600.0)
    , SvgAttr.height (gety 600.0) 
    ]     
    ([viewPlate model model.plate
        ,viewBall model model.ball]
        ++
        (viewBlocks model model.bricks))



backgroundColor : Html.Attribute Msg
backgroundColor =
    SvgAttr.fill "white"

viewPlate : Model -> Plate -> Html Msg
viewPlate model plate = 
    let
       
        x = model.plate.pos
    in
        drawreac (x,580) (150,10)  "#00CDCD"



drawBlocks : ( Float , Float ) -> Html Msg
drawBlocks  ( x , y ) = 
    drawreac (x, y) (50,50)  "#00CDCD"


viewBlocks : Model -> List Block -> List (Html Msg)
viewBlocks model blocks =
    List.map drawBlocks blocks 


drawcir : ( Float , Float ) -> Float -> String -> Html Msg
drawcir (x,y)  r color =
    
    Svg.ellipse [ SvgAttr.cx (getx x ), SvgAttr.cy (gety y ), SvgAttr.rx (getx r) , SvgAttr.ry (gety r ) ,  SvgAttr.fill color ][] 
    
   {- Svg.circle [ SvgAttr.cx (getx x xx), SvgAttr.cy (gety y yy), SvgAttr.r (getr r yy) , SvgAttr.fill color ][] -}
{- (x,y) : center point , (xx,yy) : windows size -}
viewBall : Model -> Ball -> Html Msg
viewBall model ball =
    drawcir ( Tuple.first ball.pos , Tuple.second ball.pos ) 15 "#FFEC8B"
