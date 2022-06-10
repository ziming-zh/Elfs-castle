module Update exposing (..)
import Message exposing (Msg(..))
import Model exposing (..)
import Color exposing (BallColor)
import Color exposing (BallColor(..))
import Color exposing (NormalColor(..))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick elapsed ->
            updateGame model
        Resize width height ->
            ( { model | windowsize = ( toFloat width, toFloat height ) }
            , Cmd.none
            )

        GetViewport { viewport } ->
            ( { model
                | windowsize =
                    ( viewport.width
                    , viewport.height
                    )
              }
            , Cmd.none ) 

        ArrowPressed Space ->
            ( { model | state = 
                if model.state == Playing then Paused
                else if model.state == Paused then Playing 
                else GG }, Cmd.none )

        ArrowPressed arrow ->
            ( updatePlate1 arrow model , Cmd.none )

        ArrowReleased arrow ->
            ( updatePlate2 arrow model , Cmd.none )

        Start ->
            ( model_init , Cmd.none )

        Pause ->
            ( { model | state = Paused } , Cmd.none )
        
        Resume ->
            ( { model | state = Playing } , Cmd.none )

     {-   _ ->
            ( model , Cmd.none ) -}
updateChar : ( Model, Cmd Msg) -> (Model, Cmd Msg)
updateChar (model ,cmd ) =
    let
        ball = model.ball
        energy = ball.energy
        mp = ball.mp
        newEnergyval = ball.energy.val - 25
        newMpval =
            if (ball.mp.val < (ball.mp.max+(-25))) then 
                ball.mp.val + 25
            else 
                ball.mp.max
        newBall = {ball | energy = {energy | val = newEnergyval} , mp = { mp| val = newMpval}}

    in
        ({model | ball = newBall}, cmd)
    

        

updateTime : ( Model , Cmd Msg ) -> ( Model , Cmd Msg )
updateTime ( model , cmd ) = 
    ({model| time = model.time+0.2},cmd)

updatePlate2 : ArrowKey -> Model -> Model
updatePlate2 key model = 
    if model.plate.state == Left && key == LeftKey then { model | plate = { state = None , pos =  model.plate.pos } }
    else 
    if model.plate.state == Right && key == RightKey then { model | plate = { state = None , pos = model.plate.pos } }
    else model

updatePlate1 : ArrowKey -> Model -> Model
updatePlate1 key model =
    if key == LeftKey then 
        { model | plate = { state = Left , pos = model.plate.pos } }
    else if key == RightKey then
        { model | plate = { state = Right , pos = model.plate.pos } } 
    else model

moveplate : ( Model , Cmd Msg ) -> ( Model , Cmd Msg )
moveplate ( model , cmd ) =
    let
        lpos = model.plate.pos
        npos = 
            if model.plate.state == Left then Basics.max (lpos-5) 0
            else if model.plate.state == Right then Basics.min (lpos+5) 450
            else lpos
    in
        ( { model | plate = { state = model.plate.state , pos = npos } } , Cmd.none )

updateBall : ( Model , Cmd Msg ) -> ( Model , Cmd Msg )
updateBall ( model , cmd ) = 
    let
        ( vx , vy ) = model.ball.vel
        ball = model.ball
        npos = ( (Tuple.first model.ball.pos) + vx , (Tuple.second model.ball.pos) + vy )
        nmodel = 
            if Tuple.second model.ball.pos <= 850 then
                { model | ball = { ball| pos = npos , vel = model.ball.vel } }
            else    
                { model | state = GG }
    in
        ( nmodel , Cmd.none)


updateGame : Model -> ( Model , Cmd Msg )
updateGame model = 
    if model.state == Paused || model.state == GG then
        ( model, Cmd.none)
    else
        ( model, Cmd.none)
            |> updateTime
            |> updateChar
            |> ballHitTheBrick
            |> updateBall
            |> moveplate
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
    List.concat [ List.map (pointtoline (50,0)) (getBrickPos list1) , List.map (pointtoline (0,50)) (getBrickPos list1) , 
        List.map (pointtoline(50,0)) (List.map (pointadd (0,50)) (getBrickPos list1)) , List.map (pointtoline (0,50)) (List.map (pointadd(50,0)) (getBrickPos list1)) ]

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

        nscore = ((List.length model.bricks) - (List.length nbrick))*100 + model.score


    in
        ( { model | bricks = nbrick , score = nscore } , Cmd.none )

setbottomLine : Float
setbottomLine = 780.0

updateBallColor : BallColor -> BallColor
updateBallColor color = 
    case color of
        Normal normalcolor ->
            case normalcolor of
                Purple -> Normal Yellow
                Yellow -> Normal Blue
                Blue -> Normal Purple
        _ -> color

ballHitTheBrick : ( Model , Cmd Msg ) -> ( Model, Cmd Msg)
ballHitTheBrick ( model , cmd ) =
    let
        lball =
            model.ball
        lx = Tuple.first (lball.pos)
        ly = Tuple.second (lball.pos)
        nx = lx + Tuple.first (lball.vel)
        ny = ly + Tuple.second (lball.vel)
        bottomline = setbottomLine
        -- twist the velocity direction
        lines = List.concat [[pointtoline (150,0) (model.plate.pos,bottomline)],getlines model.bricks,[{p1=(600,0),p2=(600,bottomline)},{p1=(0,0),p2=(600,0)},{p1=(0,0),p2=(0,bottomline)}]]
        line = List.concat [getlines model.bricks,[{p1=(bottomline,0),p2=(bottomline,570)},{p1=(0,0),p2=(bottomline,0)},{p1=(0,0),p2=(0,bottomline)}]]

        changedcolor = updateBallColor lball.color
        nvel1 = ( Tuple.first model.ball.vel , -(Tuple.second model.ball.vel ))
        nvel2 = ( -(Tuple.first model.ball.vel) , Tuple.second model.ball.vel)
        nball1 = { lball | pos = model.ball.pos , vel = nvel1 , color = changedcolor }
        nball2 = { lball | pos = model.ball.pos , vel = nvel2 , color = changedcolor }
    in
        if List.any (collide (lx,ly+15) (nx,ny+15) (1,0)) lines then
            ( { model | ball = nball1 } , Cmd.none ) 
                |> (updateBrike (lx,ly+15) (nx,ny+15) model.bricks)
        else  
        if List.any (collide (lx,ly-15) (nx,ny-15) (1,0)) line then
            ( { model | ball = nball1 } , Cmd.none )
                |> (updateBrike (lx,ly-15) (nx,ny-15) model.bricks)
        else  
        if List.any (collide (lx+15,ly) (nx+15,ny) (0,1)) lines then
            ( { model | ball = nball2 } , Cmd.none )
                |> (updateBrike (lx+5,ly) (nx+15,ny) model.bricks)
        else 
        if List.any (collide (lx-15,ly) (nx-15,ny) (0,1)) lines then
            ( { model | ball = nball2 } , Cmd.none )
                |> (updateBrike (lx-15,ly) (nx-15,ny) model.bricks)
        else ( model , Cmd.none )
