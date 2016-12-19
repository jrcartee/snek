module Effects exposing (generateFood, subscriptions)

import Random
import Keyboard
import Time
import Types exposing (..)
import Board exposing (positionGenerator)

-- COMMAND

randFoodItem : Random.Generator FoodItem
randFoodItem = 
    Random.map 
        (\pos -> {pos = pos})
        positionGenerator

generateFood : Cmd Msg
generateFood = 
    Random.generate NewFood
        <| Random.list 30 randFoodItem



-- SUBSCRIPTIONS


keyStream = 
    Keyboard.downs downsToMsg

tickStream =
    Time.every 
        (Time.millisecond*250)
        (\time -> Tick)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
    [ tickStream
    , keyStream
    ]



