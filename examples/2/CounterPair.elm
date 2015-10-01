module CounterPair where

import Counter
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- MODEL

type alias Model =
    { topCounter : Counter.Model
    , bottomCounter : Counter.Model
    , middleCounter : Counter.Model
    }


init : Int -> Int -> Int -> Model
init top bottom middle =
    { topCounter = Counter.init top
    , bottomCounter = Counter.init bottom
    , middleCounter = Counter.init middle
    }


-- UPDATE

type Action
    = Reset
    | Top Counter.Action
    | Bottom Counter.Action
    | Middle Counter.Action

update : Action -> Model -> Model
update action model =
  case action of
    Reset -> init 0 0 0

    Top act ->
      { model |
          topCounter <- Counter.update act model.topCounter
      }

    Bottom act ->
      { model |
          bottomCounter <- Counter.update act model.bottomCounter
      }

    Middle act ->
      { model | middleCounter <- Counter.update act model.middleCounter }



-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ Counter.view (Signal.forwardTo address Top) model.topCounter
    , Counter.view (Signal.forwardTo address Bottom) model.bottomCounter
    , Counter.view (Signal.forwardTo address Middle) model.middleCounter
    , button [ onClick address Reset ] [ text "RESET" ]
    ]
