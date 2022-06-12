module Subscriptions exposing (subscriptions)

import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp , onResize)
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
        , onKeyDown (Decode.map key1 keyCode)
        , onKeyUp (Decode.map key2 keyCode)
        ]

key1 : Int -> Msg
key1 keycode =
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
            
key2 : Int -> Msg
key2 keycode =
    case keycode of
        38 ->
            ArrowReleased UpKey
        40 ->
            ArrowReleased DownKey
        37 ->
            ArrowReleased LeftKey
        39 ->
            ArrowReleased RightKey
        32 ->
            ArrowReleased Space
        _ ->
            ArrowPressed NoKey