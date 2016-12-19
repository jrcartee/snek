module Types exposing (..)

import Keyboard exposing (KeyCode)
import Material


-- Board
type alias BoardConfig = {
    width: Int,
    height: Int,
    incr: Int
}


-- Model
type GameState = Running | Paused | Failed
type alias Model = {
    state: GameState,
    player: Player,
    food: List FoodItem,
    mdl: Material.Model
} 

-- Messages
type Msg = 
    Tick | 
    KeyDown Key | 
    NewFood (List FoodItem) | 
    Reset |
    Mdl (Material.Msg Msg) 


type Key
    = Space
    | ArrowLeft
    | ArrowRight
    | ArrowUp
    | ArrowDown
    | Unknown

keyFromKeyCode : Int -> Key
keyFromKeyCode keyCode =
    case keyCode of
        32 -> Space
        37 -> ArrowLeft
        38 -> ArrowUp
        39 -> ArrowRight
        40 -> ArrowDown
        _ -> Unknown


downsToMsg : Keyboard.KeyCode -> Msg
downsToMsg kc =
    KeyDown (keyFromKeyCode kc)


-- Shared
type alias Position = {
    x: Int,
    y: Int
}


-- Player
type SnakeBody = Empty | BodyNode Position SnakeBody
type Direction = North | East | South | West
type alias Player = {
    isGrowing: Bool,
    head: Position,
    dir: Direction,
    score: Int,
    body: SnakeBody
}


-- Food
type alias FoodItem = {
    pos: Position
}


