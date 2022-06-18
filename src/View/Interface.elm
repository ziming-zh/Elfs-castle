module View.Interface exposing (..)
import Model exposing (Model,State)
import Html exposing (..)
import Html.Attributes as HtmlAttr exposing (..)
import Html.Events exposing (onClick)
import Message exposing (Msg(..))
import Model exposing (Model,Doorstate,Door,Bricks,Ball,Plate,State)
import Svg exposing (Svg, Attribute, svg, rect, defs, filter, feGaussianBlur, feMerge, feMergeNode)
{-import Svg.Attributes exposing (width, height, viewBox, x, y, rx, fill, id, stdDeviation, result)-}
import Svg.Attributes as SvgAttr
import Color exposing (Color)
import Color exposing (BallColor(..),NormalColor(..),type2color)
getx : Float -> String
getx x  = 
    String.fromFloat (x)

gety : Float -> String
gety y = 
    String.fromFloat (y)
getr r =
    String.fromFloat (r)

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

renderBeginButton : Int -> Html Msg
renderBeginButton x =
    let
        ( txt, msg ) =
            if x == 1 then 
                ( "Start" , Begin )
            else ( "New game" , Start )
    in
    button
      [ style "background" "#B1E4F1"
        , style "border" "0"
        , style "color" "#fff"
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "18px"
        , style "height" "50px"
        , style "line-height" "45px"
        , style "width" "100px"
        , style "left" "830px"
        , style "top" "130px"
        , style "position" "absolute"
        , onClick msg
        ]
        [ text txt ]

viewBegining : Model -> (Float,Float) -> Html Msg
viewBegining model (w,h) = 
    div
    []
    [div
        []
        [ Html.img
            [ HtmlAttr.src "./assets/beginning.png"  
            , style "width" (String.fromFloat (w*3/4) ++ "px")
            , style "height"  (String.fromFloat h ++ "px")
            , style "position" "absolute"
       --     , style "opacity" (String.fromFloat (model.time/1000))
            ] []
        ]
    , div 
        [ style "opacity" (String.fromFloat (Basics.max (model.time/1000-1) 0)) ]
        [renderBeginButton 1]
    ]

viewEnding : Model -> (Float,Float) -> Html Msg
viewEnding model (w,h) =
    div
    []
    [div
        []
        [ Html.img
            [ HtmlAttr.src "./assets/beginning.png"  
            , style "width" (String.fromFloat (w*3/4) ++ "px")
            , style "height"  (String.fromFloat h ++ "px")
            , style "position" "absolute"
            , style "opacity" (String.fromFloat (model.time/1000))
            ] []
        ]
    , div 
        [ style "opacity" (String.fromFloat (Basics.max (model.time/1000-3) 0)) ]
        [renderBeginButton 2]
    , div
        [style "bottom" "80px"
        , style "color" "#34495f"
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "4px"
        , style "left" "800px"
        , style "padding" "0 30px"
        , style "position" "absolute"
        , style "top" "0" 
        , style "opacity" (String.fromFloat (Basics.max (model.time/1000-1) 0))
        ]
        [renderTxT "Final Money:" "#423C38"]
    ,   div
        [style "bottom" "80px"
        , style "color" "#34495f"
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "4px"
        , style "left" "800px"
        , style "padding" "0 30px"
        , style "position" "absolute"
        , style "top" "50px" 
        , style "opacity" (String.fromFloat (Basics.max (model.time/1000-2) 0))
        ]
        [renderTxT ((String.fromInt model.score)++"$") "#BFBC00"]
    ]

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

                Model.Begining ->
                    ( "Start" , Begin )
                
                Model.Ending -> 
                    ( "New game", Start )
    in
    button
      [ style "background" "#B1E4F1"
        , style "border" "0"
        , style "bottom" "30px"
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "18px"
        , style "height" "60px"
        , style "left" "30px"
        , style "line-height" "60px"
        , style "position" "absolute"
        , style "width" "120px"
        , onClick msg
        ]
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
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "10px"
        , style "height" "30px"
        , style "left" "30px"
        , style "line-height" "30px"
        , style "position" "absolute"
        , style "width" "120px"
        , style "display" 
            (if model.state == Model.Changing then "block"
            else "none")
        , onClick msg
        ]
        [ text txt ]


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
            , drawreac (1,321) (model.ball.mp.val*2,8) 
                (if model.ball.mp.val == model.ball.mp.max then "#FF6666"
                else
                ( case model.ball.color of 
                    Normal aa -> "#3380BC" 
                    Red bb -> "#FF6666" ))
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
            ,renderX "mp:" -145 440 "#bdc3c7",renderTip model "Press F!" -140 440 "#FF6666" 
        ]
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
