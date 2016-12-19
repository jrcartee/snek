module Board exposing (..)

import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)

import Types exposing (..)

conf : BoardConfig
conf = 
    { width = 100
    , height = 100
    , incr = 5    
    }


centerPos : Position
centerPos =
    {x = conf.width//2
    , y = conf.height//2
    }



tupleToPosition : (Int,Int) -> Position
tupleToPosition (a,b) = 
    { x = a*conf.incr
    , y = b*conf.incr 
    }

positionGenerator : Random.Generator Position
positionGenerator = 
    let
        i = conf.incr
        w = conf.width
        h = conf.height
        mapFn = tupleToPosition
    in
    Random.map mapFn 
        <| (Random.pair 
            <| Random.int 0 ((w - i) // i))
            <| Random.int 0 ((h - i) // i)


adjustPosition : Position -> Position
adjustPosition pos = 
    let 
        dx = pos.x
        dy = pos.y

        w = conf.width
        h = conf.height
        i = conf.incr
    in
        if pos.x > w then
            { pos | x = dx - (w + i) }
        else if pos.x < 0 then
            { pos | x = dx + w }
        else if pos.y > h then
            { pos | y = dy - (h + i) }
        else if pos.y < 0 then
            { pos | y = dy + h }
        else
            pos


incrPostion : Position -> Direction -> Position
incrPostion pos dir =
    case dir of
        North ->
          adjustPosition { pos | y = pos.y - conf.incr } 
        South ->
          adjustPosition { pos | y = pos.y + conf.incr } 
        West ->
          adjustPosition { pos | x = pos.x - conf.incr } 
        East ->
          adjustPosition { pos | x = pos.x + conf.incr }     


viewPosition : String -> Position -> Svg msg
viewPosition color pos =
    rect 
      [ x (toString pos.x)
      , y (toString pos.y)
      , width (toString conf.incr)
      , height (toString conf.incr)  
      , fill color
      ] []




viewX x = 
  let d = toString(x*conf.incr)
  in line [ x1 d, y1 "0", x2 d, y2 "500", stroke "#023963", strokeWidth "0.1px" ] []

viewY y =
  let d = toString (y * conf.incr)
  in line [ x1 "0", y1 d, x2 "500", y2 d, stroke "#023963", strokeWidth "0.1px" ] []
    
viewGrid = 
    let
        rangeX = List.range 0 conf.width
        rangeY = List.range 0 conf.height
    in    
    List.append (List.map viewX rangeX) (List.map viewY rangeY)
        
boxDimensions = 
    "0 0 "++ toString conf.width ++ " " ++ toString conf.height
viewBoard children = 
    
    svg 
        [ viewBox boxDimensions
        , height "500px"
        , width "500px"
        , display "inline-block"
        ]
        <| (List.append viewGrid children)
