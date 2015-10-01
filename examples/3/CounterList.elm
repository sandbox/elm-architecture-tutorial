module CounterList where

import Counter
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (toInt)


-- MODEL

type alias Model =
    { counters : List ( ID, Counter.Model )
    , nextID : ID
    , removeID : ID
    , removeIndex : Int
    }

type alias ID = Int


init : Model
init =
    { counters = []
    , nextID = 0
    , removeID = 0
    , removeIndex = 0
    }


-- UPDATE

type Action
    = Insert
    | Remove
    | RemoveById
    | RemoveByIndex
    | ModifyRemoveID ID
    | ModifyRemoveIndex Int
    | Modify ID Counter.Action


update : Action -> Model -> Model
update action model =
  case action of
    Insert ->
      let newCounter = ( model.nextID, Counter.init model.nextID )
          newCounters = model.counters ++ [ newCounter ]
      in
          { model |
              counters <- newCounters,
              nextID <- model.nextID + 1
          }

    Remove ->
      { model | counters <- List.drop 1 model.counters }

    RemoveById ->
      { model | counters <- List.filter ( \(counterid, _) -> counterid /= model.removeID) model.counters }

    RemoveByIndex ->
      { model | counters <- (List.take (model.removeIndex) model.counters) ++ (List.drop (model.removeIndex + 1) model.counters) }
--      { model | counters <- List.map snd (List.filter ( \(counterIndex, _) -> counterIndex /= model.removeIndex) (List.indexedMap (,) model.counters)) }

    ModifyRemoveID id ->
      { model | removeID <- id }

    ModifyRemoveIndex id ->
      { model | removeIndex <- id }

    Modify id counterAction ->
      let updateCounter (counterID, counterModel) =
            if counterID == id
                then (counterID, Counter.update counterAction counterModel)
                else (counterID, counterModel)
      in
          { model | counters <- List.map updateCounter model.counters }


-- VIEW
view : Signal.Address Action -> Model -> Html
view address model =
  let counters = List.map (viewCounter address) model.counters
      remove = button [ onClick address Remove ] [ text "Remove" ]
      insert = button [ onClick address Insert ] [ text "Add" ]
      removeId = input [ type' "number"
                       , on "input" targetValue (\str -> Signal.message address (ModifyRemoveID (Maybe.withDefault  0 (Result.toMaybe (toInt str))))) ] []
      removeWithId = button [ onClick address RemoveById ] [ text "Remove Specific Id" ]
      removeIndex = input [ type' "number"
                       , on "input" targetValue (\str -> Signal.message address (ModifyRemoveIndex (Maybe.withDefault  0 (Result.toMaybe (toInt str))))) ] []
      removeWithIndex = button [ onClick address RemoveByIndex ] [ text "Remove Specific Index" ]
  in
      div [] ([remove, insert, removeId, removeWithId, removeIndex, removeWithIndex ] ++ counters)


viewCounter : Signal.Address Action -> (ID, Counter.Model) -> Html
viewCounter address (id, model) =
  Counter.view (Signal.forwardTo address (Modify id)) model
