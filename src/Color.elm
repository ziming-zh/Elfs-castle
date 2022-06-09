module Color exposing (Color, rgb,toString)

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


type Color
    = Color { red : Int, green : Int, blue : Int }


rgb : Int -> Int -> Int -> Color
rgb red green blue =
    Color { red = red, green = green, blue = blue }

