module Update exposing (..)
import Message exposing (Msg(..))
import Model exposing (Model, ArrowKey(..),Bricks,Line,Block)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick elapsed ->
            updateGame model
        ArrowPressed Space ->
            ( { model | paused = not model.paused }, Cmd.none )
        _ ->
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
        x = (y1-ly)*(nx-lx)/(ny-ly) + lx
        y = (x1-lx)*(ny-ly)/(nx-lx) + ly
    in
        if y1 == y2 && a == 1 then ( x >= (Basics.min x1 x2) && x <= (Basics.max x1 x2) ) && ( x >= (Basics.min lx nx) && x <= (Basics.max lx nx) )
        else if x1 == x2 && b == 1 then ( y >= (Basics.min y1 y2) && y <= (Basics.max y1 y2) ) && ( y >= (Basics.min ly ny) && y <= (Basics.max ly ny))
        else (0 == 1)

checkbrike : ( Float , Float ) -> ( Float , Float ) -> Block -> Bool
checkbrike (lx,ly) (nx,ny) (x,y) =
    let
        lines = getlines [(x,y)]
    in
        not (List.any (collide (lx,ly) (nx,ny) (1,1)) lines)

updateBrike : ( Float , Float ) -> ( Float , Float ) -> List Block -> ( Model , Cmd Msg ) -> ( Model , Cmd Msg )
updateBrike (lx,ly) (nx,ny) list1 (model,cmd) =
    let
        nbrick = List.filter (checkbrike (lx,ly) (nx,ny)) list1

    in
        ( { model | bricks = nbrick } , Cmd.none )

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

    in
        if List.any (collide (lx,ly+15) (nx,ny+15) (1,0)) lines then
            ( { model | 
                ball = { pos = model.ball.pos , vel = ( Tuple.first model.ball.vel , -(Tuple.second model.ball.vel )) } 
              }
             , Cmd.none ) 
             |> (updateBrike (lx,ly+15) (nx,ny+15) model.bricks)
        else  if List.any (collide (lx,ly-15) (nx,ny-15) (1,0)) lines then
            ( { model | 
                ball = { pos = model.ball.pos , vel = ( Tuple.first model.ball.vel , -(Tuple.second model.ball.vel )) } 
              }
             , Cmd.none )
             |> (updateBrike (lx,ly-15) (nx,ny-15) model.bricks)
        else  if List.any (collide (lx+15,ly) (nx+15,ny) (0,1)) lines then
            ( { model | 
                ball = { pos = model.ball.pos , vel = ( -(Tuple.first model.ball.vel) , Tuple.second model.ball.vel)  } 
              }
             , Cmd.none )
             |> (updateBrike (lx+5,ly) (nx+15,ny) model.bricks)
        else if List.any (collide (lx-15,ly) (nx-15,ny) (0,1)) lines then
            ( { model | 
                ball = { pos = model.ball.pos , vel = ( -(Tuple.first model.ball.vel) , Tuple.second model.ball.vel)  } 
              }
             , Cmd.none )
             |> (updateBrike (lx-15,ly) (nx-15,ny) model.bricks)
        else ( model , Cmd.none )
