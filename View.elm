module View exposing (..)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

import Svg exposing (..)
import Svg.Attributes exposing (..)

import Material
import Material.Button as Button
import Material.Grid exposing (grid, cell, size, Device(..))
import Material.Card as Card
import Material.Layout as Layout
import Material.Scheme
import Material.Color as Color
import Material.Elevation as Elevation
import Material.Options as Options
import Material.Typography as Typo

import Types exposing (..)
import Board exposing (viewPosition, viewBoard)

-- VIEW



-- main
view : Model -> Html Msg
view model =
  Material.Scheme.topWithScheme Color.Teal Color.LightGreen <|        
    Layout.render Mdl
      model.mdl
      [ Layout.fixedHeader
      ]
      { header = [viewHeader]
      , drawer = []
      , tabs = ([], [])
      , main = [viewBody model]
      }


viewHeader : Html Msg
viewHeader =
  div 
    [Html.Attributes.style 
    [ ("font-size", "3rem")
    , ("padding", "1.5rem")
    ]] [ Html.text "Snek"]

viewBody : Model -> Html Msg
viewBody model =
  [ cell 
    [size All 8
    , Options.center
    ] 
    [ viewBoard <|
        List.concat 
          [ viewSnake model.player 
          , viewFood model.food
          ]      
    ] 
  , cell [size All 4] 
    [ if model.state == Failed then 
        viewFailure model
      else 
        viewScore model
    ]
  --, cell [size All 4] [viewFailure model]
  ] |> grid [ {-Html.Attributes.style [("padding", "50px")]-}]
    


-- State
viewFailure : Model -> Html Msg
viewFailure model =
  Card.view 
    [ Options.css "width" "100%"
    , Options.css "margin-top" "50px"
    , Elevation.e4
    ] 
    [ Card.title 
        [ Options.css "text-align" "center"
        , Options.css "font-size" "4rem"
        ]
        [Html.text "GAME OVER!"
        ]
    , Card.text 
        [ Options.center
        ] 
        [ div 
          [ Html.Attributes.style [("padding-right", "30px")]
          ] 
          [ Html.text ("Final Score: " ++ toString model.player.score)
          ]
        , Button.render Mdl [0] model.mdl
          [ Button.raised
          , Button.colored
          , Button.ripple
          , Button.onClick Reset
          ]
          [ Html.text "Reset"
          ]
        ]
    ]

viewScore model = 
  Card.view  
    [ Options.css "width" "100%"
    , Options.css "margin-top" "50px"
    , Color.background (Color.color Color.Blue Color.S300)
    , Elevation.e4
    , Options.css "display" "inline-block"
    , Options.css "padding" "0.5rem 2rem"
    -- Click
    --, Options.attribute <| Html.Events.onClick Click
    ]
    [ Card.title 
      [ Color.text Color.white
      , Options.css "font-size" "2rem" 
      ] 
      [ Html.text "Score" 
      ] 
    , Card.text 
    --, Options.span 
      [ Color.text Color.white 
      , Options.css "font-size" "3rem"
      , Options.css "font-weight" "bold"
      , Typo.right
      ] 
      [ Html.text 
        <| toString model.player.score 
      ]
    ]
    



-- Snake
viewSnakePixel : Position -> Svg msg
viewSnakePixel = viewPosition "red"


viewSnakeBody body = 
    case body of
        BodyNode pos tail ->
            viewSnakePixel pos :: (viewSnakeBody tail)            
        Empty ->
            [rect[][]]
                        
viewSnake player =
    viewSnakePixel player.head :: viewSnakeBody player.body


-- Food
viewFoodPixel food = 
  viewPosition "green" food.pos

viewFood : List FoodItem -> List (Svg msg)
viewFood list =
  List.map viewFoodPixel list


