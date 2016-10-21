module Flashcards.Components.Topics
    ( State(..)
    , TopicsMap
    , Status(..)
    , initialState
    , Action(..)
    , update
    , view
    ) where


-------------------------------------------------------------------------------
import Data.Map as M
import Flashcards.Client.Topics as Topics
import Data.Either (Either(Right, Left))
import Data.Monoid (mempty)
import Data.Tuple (Tuple(Tuple))
import Network.HTTP.Affjax (AJAX)
import Prelude ((<<<), map)
import Pux (noEffects, EffModel)
import Pux.Html (text, Html)
-------------------------------------------------------------------------------


type TopicsMap = M.Map Topics.TopicId Topics.Topic


-------------------------------------------------------------------------------
type State = {
      topics :: TopicsMap
    , status :: Status
    }


-------------------------------------------------------------------------------
data Status = NotLoaded
            | Loading
            | Loaded
            | LoadError String


-------------------------------------------------------------------------------
initialState :: State
initialState = {topics: mempty, status: NotLoaded}


-------------------------------------------------------------------------------
data Action = RefreshTopics
            | ReceiveTopics (Either String (Array Topics.Topic))


-------------------------------------------------------------------------------
update :: forall eff. Action -> State -> EffModel State Action (ajax :: AJAX | eff)
update RefreshTopics s = {
      state: s { status = Loading }
    , effects: [map ReceiveTopics Topics.getTopics]
    }
update (ReceiveTopics (Left e)) s = noEffects s { status = LoadError e }
update (ReceiveTopics (Right ts)) s = noEffects s {
      status = Loaded
    , topics = mapTopics ts
    }


-------------------------------------------------------------------------------
mapTopics :: Array Topics.Topic -> TopicsMap
mapTopics = M.fromFoldable <<< map toPair
  where
    toPair topic@(Topics.Topic t) = Tuple t.id topic


-------------------------------------------------------------------------------
view :: State -> Html Action
view {status: NotLoaded} = text "Not loaded."
view {status: Loading} = text "Loading."
view {status: LoadError e} = text e --TODO: error
view _ = text "TODO: display"
