{- detecting related functions -}
module Detector exposing (collide,getDir,getlines,getListline,getNball,getNvel,getNvel3,dis)
import Color exposing (BallColor,BallColor(..),NormalColor(..))

import Model exposing (Model,getBrickPos,Line,Ball,Bricks,Dir,Dir(..),Property)
dis : ( Float , Float ) -> ( Float , Float ) -> Float
dis (x1,y1) (x2,y2) =
    sqrt ((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1))


inside : ( Float , Float ) -> (Float , Float ) -> Bool
inside (x,y) (x1,y1) =
    dis (x,y) (x1,y1) <= 15

ck1 : ( Float , Float ) -> ( Float , Float ) -> (Float , Float ) -> Bool
ck1 (x,y) (x1,y1) (x2,y2) = 
        ( y <= y1+15 && y >= y1 && x >= x1 && x <= x2 )
    || ( inside (x,y) (x1,y1) && x+y >= x1+y1 )
    || ( inside (x,y) (x2,y2) && y-x >= y2-x2  ) 

ck2 : ( Float , Float ) -> ( Float , Float ) -> (Float , Float ) -> Bool
ck2 (x,y) (x1,y1) (x2,y2) = 
        ( y >= y1-15 && y <= y1 && x >= x1 && x <= x2 )
    || ( inside (x,y) (x1,y1) && y-x <= y1-x1)
    || ( inside (x,y) (x2,y2) && x+y <= x2+y2 )

ck3 : ( Float , Float ) -> ( Float , Float ) -> (Float , Float ) -> Bool
ck3 (x,y) (x1,y1) (x2,y2) = 
        ( x <= x1 && x >= x1-15 && y >= y1 && y <= y2 )
    || ( inside (x,y) (x1,y1) && y-x >= y1-x1 )
    || ( inside (x,y) (x2,y2) && x+y <= y2+x2 )

ck4 : ( Float , Float ) -> ( Float , Float ) -> (Float , Float ) -> Bool
ck4 (x,y) (x1,y1) (x2,y2) = 
        ( x >= x1 && x <= x1+15 && y >= y1 && y <= y2 )
    || ( inside (x,y) (x1,y1) && x+y >= x1+y1 )
    || ( inside (x,y) (x2,y2) && y-x <= y2-x2 )

ck5 : ( Float , Float ) -> ( Float , Float ) -> (Float , Float ) -> Bool
ck5 (x,y) (x1,y1) (x2,y2) = 
        ( x >= x1-15 && x <= x1+15 && y >= y1 && y <= y2 ) || inside (x,y) (x1,y1) || inside (x,y) (x2,y2)

ck6 : ( Float , Float ) -> ( Float , Float ) -> (Float , Float ) -> Bool
ck6 (x,y) (x1,y1) (x2,y2) = 
        ( y >= y1-15 && y <= y1+15 && x >= x1 && x <= x2 ) || inside (x,y) (x1,y1) || inside (x,y) (x2,y2) 

collide : ( Float , Float ) -> ( Float , Float ) -> ( Float , Float ) -> Line -> Bool
collide (lx,ly) (nx,ny) (a,b) line =
    let
        (x1,y1) = line.p1
        (x2,y2) = line.p2
    in
        if b == 0 then 
            if y1 == y2 then 
                if a == 1 then
                   ck1 (nx,ny) (x1,y1) (x2,y2) && not (ck1 (lx,ly) (x1,y1) (x2,y2))
                else 
                   ck2 (nx,ny) (x1,y1) (x2,y2) && not (ck2 (lx,ly) (x1,y1) (x2,y2))
            else False
        else 
        if a == 0 then
            if x1 == x2 then
                if b == 1 then
                   ck3 (nx,ny) (x1,y1) (x2,y2) && not (ck3 (lx,ly) (x1,y1) (x2,y2))
                else
                   ck4 (nx,ny) (x1,y1) (x2,y2) && not (ck4 (lx,ly) (x1,y1) (x2,y2))
            else False
        else 
            if x1 == x2 then
               ck5 (nx,ny) (x1,y1) (x2,y2) && not (ck5 (lx,ly) (x1,y1) (x2,y2))
            else
               ck6 (nx,ny) (x1,y1) (x2,y2) && not (ck5 (lx,ly) (x1,y1) (x2,y2))
               



getListline : Model -> ( List Line , List Line , ( List Line , List Line , ( List Line , List Line , ( List Line , List Line , List Line ) ) ) )
getListline model = 
    let
        lineb = getlines ( List.filter (\block -> (Tuple.second block) == Blue) model.bricks ) 
        
        linep = getlines ( List.filter (\block -> (Tuple.second block) == Purple) model.bricks ) 
        liney = getlines ( List.filter (\block -> (Tuple.second block) == Yellow) model.bricks ) 
        lineB = getlines ( List.filter ((\block -> (Tuple.second block) == Black || (Tuple.second block) == Color.Noth )) model.bricks )
        lineg = getlines ( List.filter (\block -> (Tuple.second block) == Grey) model.bricks )
        lineball = List.concat [lineb,linep,liney,lineg] 
        plate = [pointtoline (150,0) (model.plate.pos,780),{p1=(600,0),p2=(600,780)},{p1=(0,0),p2=(600,0)},{p1=(0,0),p2=(0,780)}]
        line = List.concat [plate,lineB,lineball]
        lines = List.concat [plate,line]
    in
        ( lineb , linep , ( liney , lineB , (lineg,lineball,(plate,line,lines) ) ) )

getNvel : Model -> ( (Float,Float) , (Float,Float) , (Float,Float)  )
getNvel model = 
    let
    
        ( vx , vy ) = model.ball.vel
        nvel1 = ( vx , -vy)
        nvel2 = ( -vx , vy)
        nvel4 = ( -vx , -vy)
    in
        ( nvel1 , nvel2 ,  nvel4 )

getDir : (Float,Float) -> (Float,Float) -> ( (List Line -> Bool) ,(List Line -> Bool) , (List Line -> Bool,List Line -> Bool) )
getDir (lx,ly) (nx,ny) = 
    let 
        up = List.any (collide (lx,ly) (nx,ny) (1,0))
        down = List.any (collide (lx,ly) (nx,ny) (-1,0)) 
        right =  List.any (collide (lx,ly)  (nx,ny) (0,-1)) 
        left =  List.any (collide (lx,ly)  (nx,ny) (0,1)) 
    in
        ( up , down , (right , left) )

getNball : Model -> Ball -> (Float,Float) -> (Float,Float) -> (Float,Float) -> (Float,Float) -> BallColor -> Property
    -> (Ball,Ball,(Ball,Ball,Ball))
getNball model lball nvel1 nvel2 nvel3 nvel4 ncolor nmp =
    let 
        nball1 = { lball | mp = nmp , pos = model.ball.pos , vel = nvel1 , color = ncolor }
            
        nball2 = { lball | mp = nmp , pos = model.ball.pos , vel = nvel2 , color = ncolor }
        nball3 = { lball | mp = nmp , pos = model.ball.pos , vel = nvel3 , color = ncolor }
        nball4 = { lball | mp = nmp , pos = model.ball.pos , vel = nvel4 , color = ncolor }
        fball = { lball | mp = nmp , pos = model.ball.pos , vel = (0,0) , color = ncolor }
    in
        (nball1,nball2,(nball3,nball4,fball))

getNvel3 : Model -> (Float,Float) -> (List Line -> Bool) -> List Line -> (Float,Float)
getNvel3 model nvel1 down plate = 
    if down plate then
        if (model.plate.state /= None) then updateSpeed nvel1 model.plate.state model.level.id
        else nvel1
    else model.ball.vel

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
            2 -> 2
            3 -> 3
            _ -> 3.5)
    in
        ( speed * cos( nangle ) , speed * sin( nangle ) )