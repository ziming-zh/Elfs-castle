module Main exposing (..)

import Browser




import Model exposing (Model,ArrowKey(..),model_init)

import Message exposing (Msg(..))
import Update exposing (update)
import View exposing (view)
import Subscriptions exposing (subscriptions)
import Task
import Browser.Dom exposing (Viewport, getViewport)
--Main


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }

init : () -> ( Model, Cmd Msg )
init a =
        ( model_init
        , Task.perform GetViewport getViewport
        )

{-
    ( model_init , Task.perform GetViewport getViewport )

    Browser.element
        { init =
            \value ->
                ( value
                    |> Decode.decodeValue Model.decode
                    |> Result.withDefault Model.initial
                , Task.perform GetViewport getViewport
                )
        , update = Update.update
        , view = View.view
        , subscriptions = subscriptions
        }
-}