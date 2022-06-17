module View.Pacman exposing (FanShape,viewFanShape)

import Debug exposing (toString)
import Html exposing (Html)
import Message exposing (Msg)
import Html exposing (..)
import Html.Attributes as HtmlAttr exposing (..)
import Svg exposing (svg, circle)
import Svg.Attributes exposing (width, cx, cy, r, fill, fillOpacity, stroke, strokeWidth, strokeDashoffset, strokeDasharray)
import Svg.Attributes exposing (zoomAndPan)
import Svg.Attributes exposing (transform)
type alias FanShape =
  { offset: Float
  , percentage: Float
  , color: String
  , pos: (Float, Float)
  }

viewFanShape : FanShape -> Html Msg
viewFanShape fanShape =
  let
    (x,y) = fanShape.pos
    strokeDashoffset_ = String.fromFloat <| 25.0 - fanShape.offset
    strokeDasharray_ = String.fromFloat fanShape.percentage ++ " " ++ (String.fromFloat <| 100.0 - fanShape.percentage)
  in
    circle
      [ cx (toString 0), cy (toString 0), r "15"
      , fill "#ffffff", fillOpacity "0.0"
      , stroke fanShape.color, strokeWidth "30", strokeDashoffset strokeDashoffset_, strokeDasharray strokeDasharray_ 
      , transform ("translate("++(toString x)++","++(toString y)++") scale (0.6,0.6)")
     ]
      []

angle : Float
angle =
    degrees 40


