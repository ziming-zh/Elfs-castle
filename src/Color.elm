module Color exposing (BallColor(..),NormalColor(..), Color,type2color, rgb,toString)

import Json.Decode as Decode
import Json.Encode as Encode
toString : Color -> String
toString (Color { red, green, blue }) =
    "rgb("
        ++ String.fromInt red
        ++ ","
        ++ String.fromInt green
        ++ ","
        ++ String.fromInt blue
        ++ ")"

type BallColor
    = Red NormalColor
    | Normal NormalColor

type NormalColor
    = Blue
    | Yellow
    | Purple
    | Black

type2color : BallColor -> String
type2color color = 
    case color of 
        Normal normalcolor -> 
            case normalcolor of 
                Blue -> "#3399FF"
                Yellow -> "#FFB266"
                Purple -> "#FF66B2"
                Black -> "#646464"
        _ -> "#FF6666"


type Color
    = Color { red : Int, green : Int, blue : Int }


rgb : Int -> Int -> Int -> Color
rgb red green blue =
    Color { red = red, green = green, blue = blue }

