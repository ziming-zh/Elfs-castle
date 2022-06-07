module Subscriptions exposing (..)

import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onResize)
import Debug exposing (toString)
import Debug exposing (toString)
import Model exposing (Model,ArrowKey(..))
import Message exposing (Msg(..))
import Json.Decode as Decode

import Html.Events exposing (keyCode)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onAnimationFrameDelta Tick
        , onResize Resize
        , onKeyDown (Decode.map key keyCode)
        ]

key : Int -> Msg
key keycode =
    case keycode of
        38 ->
            ArrowPressed UpKey

        40 ->
            ArrowPressed DownKey

        37 ->
            ArrowPressed LeftKey

        39 ->
            ArrowPressed RightKey

        32 ->
            ArrowPressed Space

        _ ->
            ArrowPressed NoKey