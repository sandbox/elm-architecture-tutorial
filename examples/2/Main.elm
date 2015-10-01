
import CounterPair exposing (init, update, view)
import StartApp.Simple exposing (start)


main =
  start
    { model = init 0 0 50
    , update = update
    , view = view
    }
