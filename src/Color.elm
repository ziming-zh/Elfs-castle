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
    | Grey
    | Noth
    | Nocolor

type2color : BallColor -> String
type2color color = 
    case color of 
        Normal normalcolor -> 
            case normalcolor of 
                Blue -> "#33CCFF"
                Yellow -> "#FFB266"
                Purple -> "#3380BC"
                Grey -> "#CCCCCC"
                Black -> "#646464"
                Nocolor -> "#FFFFFF"
                Noth -> "#ECF0F1"
        _ -> "#FF6666"


type Color
    = Color { red : Int, green : Int, blue : Int }


rgb : Int -> Int -> Int -> Color
rgb red green blue =
    Color { red = red, green = green, blue = blue }

