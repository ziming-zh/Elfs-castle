module Message exposing (Msg(..))

import Browser.Dom exposing (Viewport)
import Model exposing (ArrowKey)

type Msg
    = ArrowPressed ArrowKey
    | ArrowReleased ArrowKey
    | Tick Float
    | GetViewport Viewport
    | Resize Int Int
    | Start
    | Pause
    | Resume
