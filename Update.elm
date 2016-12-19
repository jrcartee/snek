module Update exposing (..)

import Keyboard exposing (KeyCode)
import Material

import Board exposing (incrPostion)
import Types exposing (..)
import Model exposing (init)
import Effects exposing (generateFood)


-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        KeyDown keyCode ->
            onKeyDown model keyCode
        Tick ->
            onTick model
        NewFood list ->
            ({model|food=list}, Cmd.none)
        Reset ->
            init
        Mdl msg ->
            Material.update msg model



onKeyDown : Model -> Key -> (Model, Cmd Msg)
onKeyDown model key = 
    case model.state of
        Paused ->
            (keyWhenPaused model key, Cmd.none)
        Running ->
            (keyWhenRunning model key, Cmd.none)
        Failed ->
            keyWhenFailed model key

onTick: Model -> (Model, Cmd Msg)
onTick model =
    case model.state of
        Running ->
            model 
                |> advancePlayer
                |> checkForFail 
                |> updateFood
        _ ->
            (model, Cmd.none)            


-- Player
updateBody: Bool -> Position -> SnakeBody -> SnakeBody
updateBody grow pos body = 
    case body of
        Empty ->
            if grow then 
                BodyNode pos Empty 
            else Empty
        BodyNode bPos tail ->
            BodyNode pos (updateBody grow bPos tail)


advancePlayer : Model -> Model
advancePlayer model =
    let p = model.player in
        { model | 
            player = { p |
                head = (incrPostion p.head p.dir), 
                body = (updateBody p.isGrowing p.head p.body),
                isGrowing = False
            }
        }

foodEaten : Player -> Player
foodEaten p =
    let s = p.score in
    { p | 
        score = s + 1, 
        isGrowing = True
    }



-- Food
updateFood : Model -> (Model, Cmd Msg)
updateFood model = 
    if foodToEat model then
        (eatFood model, Cmd.none)
    else if List.isEmpty model.food then
        (model, generateFood)
    else
        (model, Cmd.none)
foodToEat m =
    List.any (\i -> m.player.head == i.pos) m.food


eatFood : Model -> Model
eatFood model =
    let p = model.player in
    { model | 
        player = foodEaten p, 
        food = removeFood model.food p.head
    }   
removeFood food pos =
    List.filter (\i -> pos /= i.pos) food



-- State
togglePause : Model -> Model
togglePause model =
    if model.state == Paused then
        { model | state = Running }
    else if model.state == Running then
        { model | state = Paused }
    else
        model

bodyPosMatchesHead : Model -> SnakeBody -> Bool
bodyPosMatchesHead model body = 
    case body of
        Empty ->
            False
        BodyNode pos tail ->
            (model.player.head == pos) || bodyPosMatchesHead model tail

checkForFail : Model -> Model
checkForFail model = 
    if bodyPosMatchesHead model model.player.body then
        {model | state = Failed}
    else
        model



-- Keys
keyWhenPaused : Model -> Key -> Model
keyWhenPaused model key =
    case key of
        Space ->
            togglePause model
        _ ->
            model

keyWhenRunning : Model -> Key -> Model
keyWhenRunning model key =
    let
        p = model.player
    in case key of
        ArrowUp ->
          { model | player = { p | dir = North }}
        ArrowDown ->
          { model | player = { p | dir = South }}
        ArrowLeft ->
          { model | player = { p | dir = West }}
        ArrowRight ->
          { model | player = { p | dir = East }}
        Space -> 
            togglePause model
        _ -> 
            model

keyWhenFailed : Model -> Key -> (Model, Cmd Msg)
keyWhenFailed model key =
    case key of
        Space ->
            init
        _ ->
            (model, Cmd.none)


