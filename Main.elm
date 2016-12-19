import Html exposing (Html)

import Types exposing (Model)
import Model exposing (init)
import View exposing (view)
import Update exposing (update)
import Effects exposing (subscriptions)


main : Program Never Types.Model Types.Msg
main =
  Html.program 
    { init = init
    , view = view
    , update = update 
    , subscriptions = subscriptions
    }

