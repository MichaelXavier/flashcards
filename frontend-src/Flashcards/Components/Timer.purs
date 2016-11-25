-- | A timer that counts up. Thread a signal through with the time
-- from Signal.Time to tick the timer.
module Flashcards.Components.Timer
    ( initialState
    , view
    , update
    , State
    , Action(..)
    ) where


-------------------------------------------------------------------------------
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(Just, Nothing))
import Prelude (div, mod, show, (-), (<>), (>))
import Pux.Html (Html, text)
import Signal.Time (Time)
-------------------------------------------------------------------------------


type State = {
      started :: Maybe Time
    , elapsed :: Time
    }


-------------------------------------------------------------------------------
initialState :: State
initialState = {started: Nothing, elapsed: toNumber 0}


-------------------------------------------------------------------------------
data Action = Tick Time
            | Start Time


-------------------------------------------------------------------------------
update :: Action -> State -> State
update (Start now) s = s { started = Just now }
update (Tick now) s = case s.started of
  Just started | now > started -> s { elapsed = now - started }
  _ -> s


-------------------------------------------------------------------------------
view :: forall a. State -> Html a
view s = text (formatDuration s.elapsed)


-------------------------------------------------------------------------------
--TODO: zero-pad
formatDuration :: Time -> String
formatDuration millis = show mins <> "m" <> show secs <> "s"
  where
    secsTotal = floor millis `div` 1000
    mins = secsTotal `div` 60
    secs = secsTotal `mod` 60
