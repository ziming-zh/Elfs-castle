module Update exposing (..)
import Message exposing (Msg)
import Model exposing (Model, ArrowKey(..))

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model , Cmd.none )
{-
    case msg of
        Tick elapsed ->
            updateGame model

        GetViewport { viewport } ->
            ( { model
                | windowsize =
                    ( viewport.width
                    , viewport.height
                    )
              }
            , Cmd.none
            )

        Resize wid hei ->
            ( { model
                | windowsize =
                    ( toFloat wid
                    , toFloat hei
                    )
              }
            , Cmd.none
            )
        ArrowPressed Space ->
            ( { model | paused = not model.paused }, Cmd.none )

        ArrowPressed arrow ->
            ( move_plate arrow model, Cmd.none )
-}


move_plate : ArrowKey -> Model -> Model
move_plate keyy model =
    let
        old_plate =
            model.plate
        
        new_plate = 
            if keyy == LeftKey then
                old_plate+(-10)
            else 
            if keyy == RightKey then
                old_plate+10
            else   
                old_plate
    in
        { model | plate = new_plate }

updateGame : Model -> ( Model , Cmd Msg )
updateGame model = 
    if model.isDead || model.paused then
        ( model, Cmd.none)
    else
        ( model, Cmd.none)
        {-    |> ballHitTheBrick
            |> ballAtTheEdge
            |> ballAtTheBottom
            |> updateBall
            |> updateBrick-}

ballHitTheBrick : ( Model , Cmd Msg ) -> ( Model, Cmd Msg)
ballHitTheBrick ( model, cmd) =
    let
        mball =
            model.ball
        
        posx = Tuple.first (mball.pos)
        posy = Tuple.second (mball.pos)
        vlx = Tuple.first (mball.vel)
        vly = Tuple.second (mball.vel)
        nball = mball
        -- twist the velocity direction
       {- if posx == 0  || posx == 800 then 
            --nball-velocity-twist here
            nball = mball -}
    in
        ( { model | ball = nball }, cmd )

{-
ballAtTheEdge : ( Game, Cmd Msg ) -> ( Game, Msg )
ballAtTheEdge ( game, cmd ) = 
    let 

    in

ballAtTheBottom : ( Game, Cmd Msg ) -> ( Game, Msg )
ballAtTheBottom ( game, cmd ) = 
    let 

    in



updateBrick : ( Game, Cmd Msg ) -> ( Game, Msg )
updateBrick ( game, cmd ) = 
    let 
    -- logic to edit the List Bricks in the game properties 

    in
-}