module Update exposing (..)
import Message exposing (Msg(..))
import Model exposing (Model, ArrowKey(..),Bricks,Line)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick elapsed ->
            updateGame model
        ArrowPressed Space ->
            ( { model | paused = not model.paused }, Cmd.none )
        _ ->
            ( model , Cmd.none )
        Tick elapsed ->
            updateGame model
    case msg of

        GetViewport { viewport } ->
                    , viewport.height
                    )
              }
            , Cmd.none
            )
            ( { model
                | windowsize =
{-
                    ( viewport.width

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

updateBall : ( Model , Cmd Msg ) -> ( Model , Cmd Msg )
updateBall ( model , cmd ) = 
    let
        ( vx , vy ) = model.ball.vel

    in
        ( { model | ball =  { pos = ( (Tuple.first model.ball.pos) + vx , (Tuple.second model.ball.pos) + vy ),  
                              vel = model.ball.vel }
          } , Cmd.none)


updateGame : Model -> ( Model , Cmd Msg )
updateGame model = 
    if model.isDead || model.paused then
        ( model, Cmd.none)
    else
        ( model, Cmd.none)
            |> updateBall
            |> ballHitTheBrick
        {-    |> ballHitTheBrick
            |> ballAtTheEdge
            |> ballAtTheBottom
            |> updateBall
            |> updateBrick-}
pointtoline : ( Float , Float ) -> ( Float , Float ) -> Line
pointtoline (dx,dy) (x,y) = 
    Line (x,y) (x+dx,y+dy)

pointadd : ( Float , Float ) -> ( Float , Float ) -> ( Float , Float )
pointadd (dx,dy) (x,y) = 
    (x+dx,y+dy)

getlines : Bricks -> List Line
getlines list1 = 
    List.concat [ List.map (pointtoline (100,0)) list1 , List.map (pointtoline (0,20)) list1 , 
        List.map (pointtoline(100,0)) (List.map (pointadd (0,20)) list1) , List.map (pointtoline (0,20)) (List.map (pointadd(100,0)) list1) ]

collide : ( Float , Float ) -> ( Float , Float ) -> ( Float , Float ) -> Line -> Bool
collide (lx,ly) (nx,ny) (a,b) line =
    let
        (x1,y1) = line.p1
        (x2,y2) = line.p2
    in
    if a == 1 then
        if y1 == y2 then ((ly /= y1) && (ny == y1) && ( nx >= (Basics.min x1 x2) ) && ( nx <= (Basics.max x1 x2) ) )
        else (0==1)
    else 
    if x1 == x2 then
        ((lx /= x1) && (nx == x1) && ( ny >= (Basics.min y1 y2) ) && ( ny <= (Basics.max y1 y2) ) )
    else (0==1)

ballHitTheBrick : ( Model , Cmd Msg ) -> ( Model, Cmd Msg)
ballHitTheBrick ( model, cmd) =
    let
        lball =
            model.ball
        lx = Tuple.first (lball.pos)
        ly = Tuple.second (lball.pos)
        nx = lx + Tuple.first (lball.vel)
        ny = ly + Tuple.second (lball.vel)
        -- twist the velocity direction
        lines = List.concat [getlines model.bricks,[{p1=(1000,0),p2=(1000,500)},{p1=(0,0),p2=(1000,0)},{p1=(0,0),p2=(0,500)},{p1=(0,500),p2=(1000,500)}]]
       {- if posx == 0  || posx == 800 then 
            --nball-velocity-twist here
            nball = mball -}
    in
    --上下
        if List.any (collide (lx,ly+15) (nx,ny+15) (1,0)) lines then
            ( { model | 
                ball = { pos = model.ball.pos , vel = ( Tuple.first model.ball.vel , -(Tuple.second model.ball.vel )) } 
              }
             , Cmd.none )
        else  if List.any (collide (lx,ly+(-15)) (nx,ny+(-15)) (1,0)) lines then
            ( { model | 
                ball = { pos = model.ball.pos , vel = ( Tuple.first model.ball.vel , -(Tuple.second model.ball.vel )) } 
              }
             , Cmd.none )
        else  if List.any (collide (lx+15,ly) (nx+15,ny) (0,1)) lines then
            ( { model | 
                ball = { pos = model.ball.pos , vel = ( -(Tuple.first model.ball.vel) , Tuple.second model.ball.vel)  } 
              }
             , Cmd.none )
        else if List.any (collide (lx+(-15),ly) (nx+(-15),ny) (0,1)) lines then
            ( { model | 
                ball = { pos = model.ball.pos , vel = ( -(Tuple.first model.ball.vel) , Tuple.second model.ball.vel)  } 
              }
             , Cmd.none )
        else ( model , Cmd.none )
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