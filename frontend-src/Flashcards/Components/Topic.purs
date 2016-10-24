module Flashcards.Components.Topic
    ( Action(..)
    , State(..)
    , initialState
    , update
    , view
    ) where


-------------------------------------------------------------------------------
import Flashcards.Client.Cards as Cards
import Flashcards.Client.Topics as Topics
import Control.Monad.Except (ExceptT(ExceptT), runExceptT)
import Data.Either (Either(Right, Left))
import Data.Maybe (fromMaybe, Maybe(Just, Nothing))
import Data.Monoid (mempty)
import Data.Tuple (snd, fst, Tuple(Tuple))
import Network.HTTP.Affjax (AJAX)
import Prelude (pure, ($), bind, map)
import Pux (noEffects, EffModel)
import Pux.Html (span, (#), (##), div, (!), text, Html)
import Pux.Html.Attributes (className)
-------------------------------------------------------------------------------


data Action = RefreshTopic Topics.TopicId
            | ReceiveTopic (Either String (Maybe (Tuple Topics.Topic (Array Cards.Card))))


-------------------------------------------------------------------------------
type State = {
      topic :: Maybe Topics.Topic
    , cards :: Array Cards.Card
    }


-------------------------------------------------------------------------------
initialState :: State
initialState = { topic: Nothing, cards: mempty }


-------------------------------------------------------------------------------
update :: forall eff. Action -> State -> EffModel State Action (ajax :: AJAX | eff)
update (RefreshTopic tid) s = {
      state: s
    , effects: [getJoined]
    }
  where
    getJoined = map ReceiveTopic $ runExceptT do
      mTopic <- ExceptT (Topics.getTopic tid)
      case mTopic of
        Just topic -> do
          cards <- ExceptT (Cards.getTopicCards tid)
          pure (Just (Tuple topic cards))
        Nothing -> pure Nothing
update (ReceiveTopic (Left e)) s = noEffects s
update (ReceiveTopic (Right t)) s = noEffects s { topic = map fst t
                                                , cards = fromMaybe mempty (map snd t)
                                                }


-------------------------------------------------------------------------------
view :: State -> Html Action
view s = div ! className "container" ##
  [ topicView
  , cardsView
  ]
  where
    topicView = div ! className "row topic" #
      text "TODO: topic view"
    cardsView = div ! className "row cards" #
      div ! className "card-grid col s12" ##
        (map cardView s.cards)
    cardView (Cards.Card c) = div ! className "card" #
      div ! className "card-content" #
        span ! className "card-title" # text c.question
