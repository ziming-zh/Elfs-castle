module Update exposing (update)
import Message exposing (Msg(..))
import Model exposing (Model,ArrowKey(..),Plate,Block,Property,Ball,Line,Bricks,State(..),Dir(..),model_init,getBrickPos,model_level1,model_level2,model_level3,init_model1,init_model2,init_model3)
import Color exposing (BallColor)
import Color exposing (BallColor(..))
import Color exposing (NormalColor(..))
import Levels exposing (Condition)



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick elapsed ->
            updateGame { model | dt = elapsed }
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
            if model.state /= Changing then
                ( { model | state = 
                    if model.state == Playing then Paused
                    else if model.state == Paused then Playing 
                    else GG }, Cmd.none )
            else ( model , Cmd.none )

        ArrowPressed F ->
            ( model , Cmd.none ) |> updateState True

        ArrowPressed arrow ->
            ( updatePlate1 arrow model , Cmd.none )

        ArrowReleased arrow ->
            ( updatePlate2 arrow model , Cmd.none )

        Start ->
            ( model_init 2 , Cmd.none )

        Pause ->
            if model.state /= Changing then
                ( { model | state = Paused } , Cmd.none )
            else ( model , Cmd.none )
        
        Resume ->
            ( { model | state = Playing } , Cmd.none )

        Next ->
            case model.level.id of 
                1 -> ( init_model2 model , Cmd.none )
                2 -> ( init_model3 model , Cmd.none )
                _ -> ( model , Cmd.none )
            --  3 -> (final , Cmd.none)

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
            case ball.color of 
                Red _ ->  ball.mp.val - 0.15
                _  ->
                    Basics.min (ball.mp.val + 10) ball.mp.max
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
        npos = ( (Tuple.first model.ball.pos) + vx*model.dt/6 , (Tuple.second model.ball.pos) + vy*model.dt/6 )
        nmodel = 
            if Tuple.second model.ball.pos <= 850 then
                { model | ball = { ball| pos = npos , vel = model.ball.vel } }
            else    
                if model.live == 0 then { model | state = GG }
                else 
                    case model.level.id of 
                        1 -> model_level1 { model | live = model.live-1 }
                        2 -> model_level2 { model | live = model.live-1 }
                        3 -> model_level3 { model | live = model.live-1 }
                        _ -> model
    in
        ( nmodel , Cmd.none)

updatelevel : ( Model , Cmd Msg ) -> ( Model , Cmd Msg )
updatelevel ( model , cmd ) = 
    let
        (x,y,z) = model.level.pass
    in
        if x <= 0 && y <= 0 && z <= 0 then 
            ( { model | state = Changing } , Cmd.none  )
         --   if model.level.id == 1 
           -- then ( model_level2 model , Cmd.none )
        --  else ( model_level3 model , Cmd.none )
        else
            ( model , Cmd.none )

updateState : Bool -> ( Model , Cmd Msg ) -> ( Model , Cmd Msg )
updateState flag ( model , cmd ) =
    if flag && model.ball.mp.val == model.ball.mp.max then
        let
            lball = model.ball
            nball = 
                case lball.color of 
                    Red _ -> lball
                    Normal x -> { lball | color = Red x}
        in
            ( { model | ball = nball } , Cmd.none )
    else 
    if model.ball.mp.val <= 0 then
        case model.ball.color of 
            Red x-> 
                let
                    lball = model.ball
                    nball = { lball | color = Normal x }
                in
                    ( { model | ball = nball } , Cmd.none )
            _ -> ( model , Cmd.none )
    else
        ( model , Cmd.none )


changeview : ( Model , Cmd Msg ) -> ( Model , Cmd Msg )
changeview ( model , cmd ) =
    let
        (x,y) = model.ending.pos
        (nx,ny) = 
            if x <= 650 then
                ( x + 0.5*model.dt , y - 0.15*model.dt )
            else (x,y)
    in
        ( { model | ending = { map = model.ending.map , pos = (nx,ny) } } , Cmd.none )



updateGame : Model -> ( Model , Cmd Msg )
updateGame model = 
    if model.state == Paused || model.state == GG then
        ( model, Cmd.none)
    else
    if model.state == Changing then
        ( model , Cmd.none )
            |> changeview 
    else
        ( model, Cmd.none)
            |> updateTime
            |> (updateState False)
            |> updateChar
            |> ballHitTheBrick
            |> updateBall
            |> moveplate
            |> updatelevel
            
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

dis : ( Float , Float ) -> ( Float , Float ) -> Float
dis (x1,y1) (x2,y2) =
    sqrt ((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1))

collide : ( Float , Float ) -> ( Float , Float ) -> Line -> Bool
collide (nx,ny) (a,b) line =
    let
        (x1,y1) = line.p1
        (x2,y2) = line.p2
        inside1 = dis (x1,y1) (nx,ny) <= 25
        inside2 = dis (x2,y2) (nx,ny) <= 25
    in
        if b == 0 then 
            if y1 == y2  then 
                if a == 1 then
                   ( ny <= y1+25 && ny >= y1 && nx >= x1 && nx <= x2 )
                || ( inside1 && nx+ny >= x1+y1 )
                || ( inside2 && ny-nx >= y2-x2  )
                else 
                   ( ny >= y1-25 && ny <= y1 && nx >= x1 && nx <= x2 )
                || ( inside1 && ny-nx <= y1-x1)
                || ( inside2 && nx+ny <= x2+y2 )
            else False
        else 
        if a == 0 then
            if x1 == x2 then
                if b == 1 then
                   ( nx <= x1 && nx >= x1-25 && ny >= y1 && ny <= y2 )
                || ( inside1 && ny-nx >= y1-x1 )
                || ( inside2 && nx+ny <= y2+x2 )
                else
                   ( nx >= x1 && nx <= x1+25 && ny >= y1 && ny <= y2 )
                || ( inside1 && nx+ny >= x1+y1 )
                || ( inside2 && ny-nx <= y2-x2 )
            else False
        else 
            if x1 == x2 then
               ( nx >= x1-25 && nx <= x1+25 && ny >= y1 && ny <= y2 )
            || inside1 || inside2
            else
               ( ny >= y1-25 && ny <= y1+25 && nx >= x1 && nx <= x2 )
            || inside1 || inside2

updateSpeed : ( Float , Float ) -> Dir -> Int -> ( Float , Float )
updateSpeed (lvx,lvy) dir id =
    let
        k = lvx/lvy
        angle = 
            if k >= 1.1 then -7*pi/8
            else if k >= 0.9 then -6*pi/8
            else if k >= 0.1 then -5*pi/8
            else if k >= -0.1 then -4*pi/8
            else if k >= -0.9 then -3*pi/8
            else if k >= -1.1 then -2*pi/8
            else -pi/8
        nangle =
            if dir == Right then Basics.min(angle+pi/8) (-2*pi/8)
            else Basics.max (angle-pi/8) (-6*pi/8)
        speed = (sqrt 2) * (case id of 
            1 -> 3
            2 -> 3.5
            3 -> 4
            _ -> 4)
    in
        ( speed * cos( nangle ) , speed * sin( nangle ) )


checkbrike : BallColor -> ( Float , Float ) -> Block -> Bool
checkbrike color (nx,ny) (x,y) =
    let
        lines = getlines [(x,y)]
    in
    case color of 
        Red colorr ->
            not (List.any (collide (nx,ny) (1,1)) lines)
        _ ->
            if y /= Black then
                not (List.any (collide (nx,ny) (1,1)) lines)
            else True

updateBrike : ( Float , Float ) -> List Block -> ( Model , Cmd Msg ) -> ( Model , Cmd Msg )
updateBrike (nx,ny) list1 (model,cmd) =
    let
        nbrick = List.filter (checkbrike model.ball.color (nx,ny)) list1
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
                Black -> Normal Black
                _ -> Normal Nocolor
        _ -> color

updatePass : Model -> ( List Line -> Bool ) -> List Line -> List Line -> List Line -> Condition
updatePass model check liney lineb linep =
    let 
        ( yellow , blue , purple ) = model.level.pass
        nyellow = 
            if check liney then 
                if model.ball.color /= Normal Blue && model.ball.color /= Normal Purple then (yellow-2)
                else (yellow-1)
            else yellow
        nblue = 
            if check lineb then 
                if model.ball.color /= Normal Yellow && model.ball.color /= Normal Purple then (blue-2)
                else (blue-1)
            else blue
        npurple = 
            if check linep then 
                if model.ball.color /= Normal Yellow && model.ball.color /= Normal Blue then (purple-2)
                else (purple-1)
            else purple
    in
        (nyellow,nblue,npurple)

ck : Condition -> Condition -> Bool
ck (ny,nb,np) (y,b,p) =
    ( y-ny>=2 || b-nb>=2 || p-np >= 2)

ballHitTheBrick : ( Model , Cmd Msg ) -> ( Model, Cmd Msg)
ballHitTheBrick ( model , cmd ) =
    let
        lball =
            model.ball
        ( lx , ly ) = ( Tuple.first (lball.pos) , Tuple.second (lball.pos) )

        ( nx , ny ) = ( lx + (Tuple.first (lball.vel))*model.dt/6 , ly + (Tuple.second (lball.vel))*model.dt/6 )
        bottomline = setbottomLine
        -- twist the velocity direction
        lineb = getlines ( List.filter (\block -> (Tuple.second block) == Blue) model.bricks ) 
        
        linep = getlines ( List.filter (\block -> (Tuple.second block) == Purple) model.bricks ) 
        liney = getlines ( List.filter (\block -> (Tuple.second block) == Yellow) model.bricks ) 
        lineB = getlines ( List.filter (\block -> (Tuple.second block) == Black) model.bricks ) 
        lineball = List.concat [lineb,linep,liney] 
        plate = [pointtoline (150,0) (model.plate.pos,780),{p1=(600,0),p2=(600,780)},{p1=(0,0),p2=(600,0)},{p1=(0,0),p2=(0,780)}]
        line = List.concat [plate,lineB,lineball]
        lines = List.concat [plate,line]
        --lines = List.concat [[pointtoline (600,0) (0,780)],line]
        nvel1 = ( Tuple.first model.ball.vel , -(Tuple.second model.ball.vel ))
        nvel2 = ( -(Tuple.first model.ball.vel) , Tuple.second model.ball.vel)
        nvel4 = ( -(Tuple.first model.ball.vel) , -(Tuple.second model.ball.vel ))
        nvel3 = 
            if down plate then
                if (model.plate.state /= None) then updateSpeed nvel1 model.plate.state model.level.id
                else nvel1
            else model.ball.vel
        up = List.any (collide (nx,ny) (1,0))
        down = List.any (collide (nx,ny) (-1,0)) 
        right =  List.any (collide (nx,ny) (0,-1)) 
        left =  List.any (collide (nx,ny) (0,1)) 
        alldir = (\x -> ( up x || down x || right x || left x ) )
        npass = updatePass model alldir liney lineb linep
        nlevel = { id = model.level.id , map = model.level.map , pass = npass , speed = model.level.speed }
        ncolor = 
            case lball.color of
                Red _-> model.ball.color
                _ -> 
                    if ck npass model.level.pass then updateBallColor lball.color
                    else lball.color
        nmp = 
            case model.ball.color of 
                Red _-> model.ball.mp
                _ ->
                    if ck npass model.level.pass then  Property (Basics.min (model.ball.mp.val+10) model.ball.mp.max) model.ball.mp.max
                    else if alldir lineball then Property (Basics.min (model.ball.mp.val+2.5) model.ball.mp.max) model.ball.mp.max
                    else model.ball.mp
        nball1 = { lball | mp = nmp , pos = model.ball.pos , vel = nvel1 , color = ncolor }
            
        nball2 = { lball | mp = nmp , pos = model.ball.pos , vel = nvel2 , color = ncolor }
        nball3 = { lball | mp = nmp , pos = model.ball.pos , vel = nvel3 , color = ncolor }
        nball4 = { lball | mp = nmp , pos = model.ball.pos , vel = nvel4 , color = ncolor }
        fball = { lball | mp = nmp , pos = model.ball.pos , vel = (0,0) , color = ncolor }
    in
        (if down plate then ( { model | level = nlevel , ball = nball3 } , Cmd.none )
        else
        if up lines && ( left lines || right lines ) then ( { model | level = nlevel , ball = nball4 } , Cmd.none )
        else 
        if down plate && ( left plate || right plate ) then ( { model | level = nlevel , ball = nball4 } , Cmd.none )
        else 
        if up line then ( { model | level = nlevel , ball = nball1 } , Cmd.none ) 
        else  
        if down lines then ( { model | level = nlevel , ball = nball1 } , Cmd.none )
        else  
        if right lines then ( { model | level = nlevel , ball = nball2 } , Cmd.none )
        else 
        if left lines then ( { model | level = nlevel , ball = nball2 } , Cmd.none )
        else ( model , Cmd.none ))
            |> (updateBrike (nx,ny) model.bricks)