module Model exposing (..)

import Material

import Types exposing (..)
import Effects exposing (generateFood)
import Board exposing (centerPos)

-- MODEL


init : ( Model, Cmd Msg )
init = 
    (emptyModel, generateFood)


initPlayer : Player
initPlayer = 
    {score = 0
    ,head = centerPos
    ,dir = East
    ,body = Empty
    ,isGrowing = False
    }

initFood : List FoodItem
initFood =
    []
    
emptyModel : Model
emptyModel = { 
    player = initPlayer,    
    state = Paused,
    food = initFood,
    mdl = Material.model
  }
