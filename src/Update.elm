module Update exposing (update)
import Message exposing (Msg(..))
import Model exposing (Model,ArrowKey(..),Block,Property,Line,Bricks,State(..),Dir(..),model_init,init_model1,model_level1,model_level2,model_level3,init_model2,init_model3)
import Color exposing (BallColor)
import Color exposing (BallColor(..))
import Color exposing (NormalColor(..))
import Levels exposing (Condition)
import Html exposing (q)
import Browser.Dom exposing (getViewport)
import Detector exposing (collide,getDir,getlines,getListline,getNball,getNvel,getNvel3,dis)
import Task
import Levels exposing (Level)



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
            if model.state == Begining || model.state == Ending then 
                ( model , Cmd.none )
            else
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
            ( model_init 2 , Task.perform GetViewport getViewport )

        Pause ->
            if model.state /= Changing && model.state /= Ending && model.state /= Begining then
                ( { model | state = Paused } , Cmd.none )
            else ( model , Cmd.none )
        
        Resume ->
            ( { model | state = Playing } , Cmd.none )

        Next ->
            case model.level.id of 
                1 -> ( init_model2 model , Cmd.none )
                2 -> ( init_model3 model , Cmd.none )
                3 -> ( { model | state = Ending , time = 0 } , Cmd.none )
                _ -> ( model , Cmd.none )

        Begin -> 
            ( init_model1 model , Cmd.none )
        
            --  3 -> (final , Cmd.none)

     {-   _ ->
            ( model , Cmd.none ) -}
updateChar : ( Model, Cmd Msg) -> (Model, Cmd Msg)
updateChar (model ,cmd ) =
    let
        ball = model.ball
        energy = ball.energy
        mp = ball.mp
        newMpval =
            case ball.color of 
                Red _ ->  ball.mp.val - 0.15*model.dt/6
                _  ->
                    Basics.min (ball.mp.val + 0.01*model.dt/6) ball.mp.max
        newBall = {ball | mp = { mp| val = newMpval}}

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
            if model.plate.state == Left then Basics.max (lpos-5*model.dt/6) 0
            else if model.plate.state == Right then Basics.min (lpos+5*model.dt/6) 450
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

updateScore : ( Model , Cmd Msg ) -> ( Model , Cmd Msg )
updateScore ( model , cmd ) = 
    let
        brick = model.bricks
        nscore  = model.score +
                ( List.length ( List.filter ( \x -> ( Tuple.second x == Purple ) ) brick ) ) * 1000
            +   ( List.length ( List.filter ( \x -> ( Tuple.second x == Yellow ) ) brick ) ) * 3000
            +   ( List.length ( List.filter ( \x -> ( Tuple.second x == Blue ) ) brick ) ) * 5000
    in
        ( { model | score = nscore } , Cmd.none )

deledoor : Bricks -> Bricks 
deledoor brick =
    List.filter (\x -> (Tuple.second x /= Noth)) brick

updatelevel : ( Model , Cmd Msg ) -> ( Model , Cmd Msg )
updatelevel ( model , cmd ) = 
    let
        (x,y,z) = model.level.pass
    in
        if x <= 0 && y <= 0 && z <= 0 then 
            if model.door.state /= Model.Open then
                ( { model | door = { state = Model.Open  , time = 0 } , bricks = deledoor model.bricks} , Cmd.none  )
            else ( model , Cmd.none )    
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
        a = 1.0*13/180000
        b = -13/30
        t0 = (-b-Basics.sqrt(b*b-4*a*x))/(2*a)
        v0 = 13/30-650/1500/3000*t0
        v1 = v0-model.dt*650/1500/3000
        dx = (v0+v1)*model.dt/2
        (nx,ny) = 
            if x <= 650 then
                ( x + dx , y - dx )
            else (x,y)
    in
        ( { model | ending = { map = model.ending.map , pos = (nx,ny) } } , Cmd.none )

checkend : ( Model , Cmd Msg ) -> ( Model , Cmd Msg )
checkend ( model , cmd ) = 
    let
        (x,y) = model.ball.pos
        nx = 300
        ny = 
            case model.level.id of 
                2 -> 50
                _ -> 150
        
    in
    if model.door.state == Model.Closed then ( model , Cmd.none )
    else
        if dis (x,y) (nx,ny) <= 25 then 
            ( { model | state = Changing , door = { state = Model.Open , time = model.door.time + model.dt } } , Cmd.none )
                |> updateScore
        else 
            ( { model | door = { state = Model.Open , time = model.door.time + model.dt } } , Cmd.none )

updateGame : Model -> ( Model , Cmd Msg )
updateGame model = 
    if model.state == Changing then
        ( model , Cmd.none )
            |> changeview 
    else
    if model.state == Playing then
        ( model, Cmd.none)
            |> updateTime
            |> (updateState False)
            |> updateChar
            |> ballHitTheBrick
            |> updateBall
            |> moveplate
            |> updatelevel
            |> checkend
    else
    if model.state == Begining || model.state == Ending then
        ( { model | time = model.time + model.dt } , Cmd.none )
    else
        ( model, Cmd.none)
        {-    |> ballHitTheBrick
            |> ballAtTheEdge
            |> ballAtTheBottom
            |> updateBall
            |> updateBrick-}





checkbrike : Int -> BallColor -> ( Float , Float ) -> ( Float , Float ) -> Block -> Bool
checkbrike k color (lx,ly) (nx,ny) (x,y) =
    let
        lines = getlines [(x,y)]
        flag = 
            case color of 
                Red colorr ->
                    not (List.any (collide (lx,ly) (nx,ny) (1,1)) lines)
                _ ->
                    if y /= Black then
                        not (List.any (collide (lx,ly)  (nx,ny) (1,1)) lines)
                    else True
        nflag = if k == 0 then flag
                else not flag
    in
        if y == Color.Noth then True
        else nflag

updateBrike : ( Float , Float ) -> ( Float , Float ) -> List Block -> ( Model , Cmd Msg ) -> ( Model , Cmd Msg )
updateBrike (lx,ly) (nx,ny) list1 (model,cmd) =
    let
        nbrick = List.filter (checkbrike 0 model.ball.color (lx,ly) (nx,ny)) list1
        ggbrick = List.filter ( checkbrike 1 model.ball.color (lx,ly) (nx,ny)) list1
        nscore = model.score + ((List.length list1)-(List.length nbrick))*100
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
        lball = model.ball
        ( lx , ly ) = lball.pos
        ( vx , vy ) = lball.vel
        ( nx , ny ) = ( lx + vx*model.dt/6 , ly + vy*model.dt/6 )
        -- twist the velocity direction
        ( lineb , linep , ( liney , lineB , ( lineg, lineball,(plate,line,lines) ) ) ) = getListline model
        ( nvel1 , nvel2 , nvel4 ) = getNvel model
        ( up , down , (right , left) ) = getDir (lx,ly) (nx,ny)
        nvel3 = getNvel3 model nvel1 down plate 
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
        (nball1,nball2,(nball3,nball4,fball)) = getNball model model.ball nvel1 nvel2 nvel3 nvel4 ncolor nmp 
    in
        hitResult model plate line lines nlevel nball1 nball2 nball3 nball4 fball

hitResult : Model -> List Line -> List Line -> List Line -> Level ->  Model.Ball  ->  Model.Ball  ->  Model.Ball  ->  Model.Ball  ->  Model.Ball -> (Model, Cmd Msg)
hitResult model plate line lines nlevel nball1 nball2 nball3 nball4 fball= 
    let
        ( lx , ly ) = model.ball.pos
        ( vx , vy ) = model.ball.vel
        ( nx , ny ) = ( lx + vx*model.dt/6 , ly + vy*model.dt/6 )
        ( up , down , (right , left) ) = getDir (lx,ly) (nx,ny)
        
    in
        (if down plate then ( { model | level = nlevel , ball = nball3 } , Cmd.none )
            else if down plate && ( left plate || right plate ) then ( { model | level = nlevel , ball = nball4 } , Cmd.none )
            else if up line then ( { model | level = nlevel , ball = nball1 } , Cmd.none ) 
            else if down lines then ( { model | level = nlevel , ball = nball1 } , Cmd.none )
            else if right lines then ( { model | level = nlevel , ball = nball2 } , Cmd.none )
            else if left lines then ( { model | level = nlevel , ball = nball2 } , Cmd.none )
            else ( model , Cmd.none ))
                |> (updateBrike (lx,ly) (nx,ny) model.bricks)