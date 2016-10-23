module Flashcards.Components.Topic
    ( Action(..)
    , State(..)
    , initialState
    , update
    , view
    ) where


-------------------------------------------------------------------------------
import Flashcards.Client.Topics as Topics
import Data.Either (Either(Right, Left))
import Data.Maybe (Maybe(Nothing))
import Network.HTTP.Affjax (AJAX)
import Prelude (map)
import Pux (noEffects, EffModel)
import Pux.Html (text, Html)
-------------------------------------------------------------------------------


data Action = RefreshTopic Topics.TopicId
            | ReceiveTopic (Either String (Maybe Topics.Topic))


-------------------------------------------------------------------------------
type State = {
      topic :: Maybe Topics.Topic
    }

--TODO: status

-------------------------------------------------------------------------------
initialState :: State
initialState = { topic: Nothing }


-------------------------------------------------------------------------------
update :: forall eff. Action -> State -> EffModel State Action (ajax :: AJAX | eff)
update (RefreshTopic tid) s = {
      state: s
    , effects: [map ReceiveTopic (Topics.getTopic tid)]
    }
update (ReceiveTopic (Left e)) s = noEffects s
update (ReceiveTopic (Right t)) s = noEffects s { topic = t}


-------------------------------------------------------------------------------
view :: State -> Html Action
view _ = text "TODO"
