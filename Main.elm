import Random
import Html exposing (Html, button, div, text, h1)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Keyboard exposing (KeyCode)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, millisecond)


main =
  Html.program 
    { init = init
    , view = view
    , update = update 
    , subscriptions = subscriptions
    }


-- MODEL
type alias Position = {
    x: Int,
    y: Int
}

type alias FoodItem = {
    pos: Position
}

type SnakeBody = Empty | BodyNode Position SnakeBody

type alias Player = {
    isGrowing: Bool,
    head: Position,
    dir: Direction,
    score: Int,
    body: SnakeBody
}
type GameState = Running | Paused | Failed
type Direction = North | East | South | West


type alias Model = {
    state: GameState,
    player: Player,
    food: List FoodItem
} 


(gameWidth, gameHeight, incr) = (100, 100, 5)

initPlayer : Player
initPlayer = 
    {score = 0
    ,head = {x = gameWidth//2, y = gameHeight//2}
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
    food = initFood
  }

init : ( Model, Cmd Msg )
init = 
    (emptyModel, Random.generate NewFood generateFood)

-- UPDATE

rand : Int -> Random.Generator Int
rand i = Random.int 0 ((i-incr)//incr)

toFoodItem : (Int,Int) -> FoodItem
toFoodItem (a,b) = 
    { pos = {x = a*incr, y = b*incr } }

randFoodItem : Random.Generator FoodItem
randFoodItem = 
    Random.map toFoodItem (
        Random.pair (rand gameWidth) (rand gameHeight))

generateFood : Random.Generator (List FoodItem)
generateFood = Random.list 30 randFoodItem



updateHeadPostion : Position -> Direction -> Position
updateHeadPostion pos dir =
    case dir of
        North ->
          adjustPosition { pos | y = pos.y - incr } 
        South ->
          adjustPosition { pos | y = pos.y + incr } 
        West ->
          adjustPosition { pos | x = pos.x - incr } 
        East ->
          adjustPosition { pos | x = pos.x + incr }     


adjustPosition : Position -> Position
adjustPosition pos = 
    let 
        dx = pos.x
        dy = pos.y
    in
        if pos.x > gameWidth then
            { pos | x = dx - (gameWidth + incr) }
        else if pos.x < 0 then
            { pos | x = dx + gameWidth }
        else if pos.y > gameHeight then
            { pos | y = dy - (gameHeight + incr) }
        else if pos.y < 0 then
            { pos | y = dy + gameHeight }
        else
            pos


updateBodyPosition : Model -> Model
updateBodyPosition model =
    let
        player = model.player
        head = model.player.head
        body = model.player.body
        grow = model.player.isGrowing
    in
        {model | player = { player | body = (updateBody grow head body) } }

    
updateBody: Bool -> Position -> SnakeBody -> SnakeBody
updateBody grow pos snek = 
    case snek of
        Empty ->
            if grow then BodyNode pos Empty else Empty
        BodyNode bPos tail ->
            BodyNode pos (updateBody grow bPos tail)





posMatchesHead : Model -> Position -> Bool
posMatchesHead model pos = 
    model.player.head == pos

posDoesntMatchHead : Model -> Position -> Bool
posDoesntMatchHead model pos = 
    model.player.head /= pos


foodPosMatchesHead : Model -> FoodItem -> Bool
foodPosMatchesHead model item = 
    posMatchesHead model item.pos

foodPosDoesntMatchHead: Model -> FoodItem -> Bool
foodPosDoesntMatchHead model item = 
    posDoesntMatchHead model item.pos


bodyPosMatchesHead : Model -> SnakeBody -> Bool
bodyPosMatchesHead model body = 
    case body of
        Empty ->
            False
        BodyNode pos tail ->
            (posMatchesHead model pos) || bodyPosMatchesHead model tail





eatFood : Model -> Model
eatFood model =
    let 
        oldPlayer = model.player
        playerScore = oldPlayer.score
    in
        {model | 
            player = { oldPlayer | score = playerScore + 1, isGrowing = True},
            food = List.filter (foodPosDoesntMatchHead model) model.food}


updateFood : Model -> (Model, Cmd Msg)
updateFood model = 
    if List.any (foodPosMatchesHead model) model.food then
        (eatFood model, Cmd.none)
    else if List.isEmpty model.food then
        (model, Random.generate NewFood generateFood)
    else
        (model, Cmd.none)

checkForFail : Model -> Model
checkForFail model = 
    if bodyPosMatchesHead model model.player.body then
        {model | state = Failed}
    else
        model

onTick: Model -> (Model, Cmd Msg)
onTick model =
    let 
        p = model.player
        currPos = p.head
    in
        if model.state == Paused || model.state == Failed then
            (model, Cmd.none)
        else
            { model | player = { p | 
                head = (updateHeadPostion p.head p.dir), 
                body = (updateBody p.isGrowing p.head p.body),
                isGrowing = False
            }}
                |> checkForFail 
                |> updateFood
            
            

type Key
    = Space
    | ArrowLeft
    | ArrowRight
    | ArrowUp
    | ArrowDown
    | Unknown

keyFromCode : Int -> Key
keyFromCode keyCode =
    case keyCode of
        32 -> Space
        37 -> ArrowLeft
        38 -> ArrowUp
        39 -> ArrowRight
        40 -> ArrowDown
        _ -> Unknown


onKeyDown : KeyCode -> Model -> Model
onKeyDown keyCode model = 
    let
        key = keyFromCode keyCode
        oldPlayer = model.player
    in
        if key == Space then
            if model.state == Paused then
                { model | state = Running }
            else if model.state == Running then
                { model | state = Paused }
            else
                model
        else if model.state == Running then
            case key of
                ArrowUp ->
                  { model | player = { oldPlayer | dir = North }}

                ArrowDown ->
                  { model | player = { oldPlayer | dir = South }}

                ArrowLeft ->
                  { model | player = { oldPlayer | dir = West }}

                ArrowRight ->
                  { model | player = { oldPlayer | dir = East }}

                Space -> model
                Unknown -> model
        else
            model



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        KeyDown keyCode ->
            (onKeyDown keyCode model, Cmd.none)
        Tick ->
            onTick model
        NewFood list ->
            ({model|food=list}, Cmd.none)
        Reset ->
            init



-- SUBSCRIPTIONS
type Msg = 
    Tick | KeyDown KeyCode | NewFood (List FoodItem) | Reset

timeToTick: Time -> Msg
timeToTick time = Tick


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
    [ Time.every (millisecond*250) timeToTick
    , Keyboard.downs KeyDown
    ]




-- VIEW

viewBoxSize = "0 0 "++ toString gameWidth ++ " " ++ toString gameHeight

viewGridLine delta = 
  let
    d = toString(delta*incr)
  in
    [
      line [ x1 "0", y1 d, x2 "500", y2 d, stroke "#023963", strokeWidth "0.1px" ] [],
      line [ x1 d, y1 "0", x2 d, y2 "500", stroke "#023963", strokeWidth "0.1px" ] []
    ]

viewSnakeHead pos =
  rect 
    [ x (toString pos.x)
    , y (toString pos.y)
    , width (toString incr)
    , height (toString incr)  
    , fill "red"
    ] []

viewSnakeBody body = 
    case body of
        BodyNode pos tail ->
            rect 
              [ x (toString pos.x)
              , y (toString pos.y)
              , width (toString incr)
              , height (toString incr)  
              , fill "red"
              ] []  :: (viewSnakeBody tail)
            
        Empty ->
            [rect[][]]
                        
viewSnake player =
    viewSnakeHead player.head :: viewSnakeBody player.body

viewFood food = 
  rect 
    [ x (toString food.pos.x)
    , y (toString food.pos.y)
    , width (toString incr)
    , height (toString incr)  
    , fill "green"
    ] []

viewFailure model =
    if model.state == Failed then
        div [] [
            h1[][Html.text "GAME OVER!"],
            button [onClick Reset][Html.text "reset"]
        ]
    else
        div [][]


view : Model -> Html Msg
view model =
  let 
    range = List.range 0 gameWidth
  in
      div [Html.Attributes.style [("padding", "50px")]]
        [ div 
            [ Html.Attributes.style [("padding", "50px")]
            ] [ Html.text (toString model.player.score) ],
          viewFailure model,
          svg [ viewBox viewBoxSize, height "500px", width "500px"] <|
            List.append (viewSnake model.player)
            (List.append (List.map viewFood model.food) (List.concat <| List.map viewGridLine range))
                
        ]