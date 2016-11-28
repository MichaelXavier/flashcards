module Flashcards.Components.TopicTest
    ( State(..)
    , Action(..)
    , update
    , view
    ) where


-------------------------------------------------------------------------------
import Flashcards.Client.Cards as Cards
import Flashcards.Client.Topics as Topics
import Flashcards.Client.Common (Entity)
import Pux (EffModel, noEffects)
import Pux.Html (Html, text)
import Signal.Time (Time)
-------------------------------------------------------------------------------


type State = {
      topic :: (Entity Topics.Topic)
    , cards :: Array Cards.Card
    }


-------------------------------------------------------------------------------
data Action = StartTest
            | Tick Time


-------------------------------------------------------------------------------
update :: forall eff. Action -> State -> EffModel State Action eff
update _ s = noEffects s


-------------------------------------------------------------------------------
view :: State -> Html Action
view _ = text "TODO"
