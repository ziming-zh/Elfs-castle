module View.Pacman exposing (FanShape,viewFanShape)

import Svg exposing (Svg, path)
import Model exposing (Model)
import Debug exposing (toString)
import Html exposing (Html)
import Message exposing (Msg)
import Html exposing (div)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import List
import Maybe
import Svg exposing (svg, circle)
import Svg.Attributes exposing (viewBox, width, cx, cy, r, fill, fillOpacity, stroke, strokeWidth, strokeDashoffset, strokeDasharray)
import Color exposing (Color)

type alias FanShape =
  { offset: Float
  , percentage: Float
  , color: String
  , pos: (Float, Float)
  }

viewFanShape : FanShape -> Html Msg
viewFanShape fanShape =
  let
    strokeDashoffset_ = String.fromFloat <| 25.0 - fanShape.offset
    strokeDasharray_ = String.fromFloat fanShape.percentage ++ " " ++ (String.fromFloat <| 100.0 - fanShape.percentage)
  in
    circle
      [ cx (toString (Tuple.first fanShape.pos)), cy (toString (Tuple.second fanShape.pos)), r "15"
      , fill "#ffffff", fillOpacity "0.0"
      , stroke fanShape.color, strokeWidth "30", strokeDashoffset strokeDashoffset_, strokeDasharray strokeDasharray_ ]
      []


angle : Float
angle =
    degrees 40


mouthSpeed : Int
mouthSpeed =
    20

