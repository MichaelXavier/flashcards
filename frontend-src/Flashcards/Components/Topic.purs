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
import Data.Array ((:), singleton)
import Data.Either (Either(Right, Left))
import Data.Maybe (isJust, maybe, fromMaybe, Maybe(Just, Nothing))
import Data.Monoid ((<>), mempty)
import Data.Tuple (snd, fst, Tuple(Tuple))
import Flashcards.Client.Common (Entity(Entity))
import Network.HTTP.Affjax (AJAX)
import Prelude (const, (<<<), pure, ($), bind, map)
import Pux (noEffects, EffModel)
import Pux.Html (form, button, span, (#), (##), div, (!), text, Html)
import Pux.Html.Attributes (type_, disabled, className)
import Pux.Html.Events (onSubmit, onClick)
-------------------------------------------------------------------------------


data Action = RefreshTopic Topics.TopicId
            | ReceiveTopic (Either String (Maybe (Tuple (Entity Topics.Topic) (Array (Entity Cards.Card)))))
            | NewCard
            | SaveCard


-------------------------------------------------------------------------------
type State = {
      topic :: Maybe (Entity Topics.Topic)
    , cards :: Array (Entity Cards.Card)
    , newCard :: Maybe Cards.Card
    }


-------------------------------------------------------------------------------
initialState :: State
initialState = { topic: Nothing, cards: mempty, newCard: Nothing }


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
update NewCard (s@{topic: Just (Entity e)}) = noEffects (s { newCard = Just (Cards.newCard e.id) })
update NewCard s = noEffects s -- should not be possible. noop
update SaveCard s = noEffects s { newCard = Nothing } --TODO actually save


-------------------------------------------------------------------------------
view :: State -> Html Action
view s = div ! className "container" ##
  [ topicView
  , cardsView
  ]
  where
    creatingCard = isJust s.newCard
    topicView = div ! className "row topic" ##
      maybe noTopic (singleton <<< topicView') s.topic
    noTopic = []
    topicView' (Entity {val: Topics.Topic t}) = div ! className "card col s11" ##
        [ span ! className "card-title" # text t.title
        , div ! className "card-action" #
            button ! className "btn" !
              disabled creatingCard !
              onClick (const NewCard) #
              text "Add Card"
        ]
    newCardView c = form ! className "card new-card"
                         ! onSubmit (const SaveCard) ##
      [ text "TODO: card form"
      , div ! className "card-action" #
          button ! type_ "submit" !
                   className "btn" #
            text "Save"
      ]
    cardsView = div ! className "row cards" #
      div ! className "card-grid col s12" ##
        case s.newCard of
          Just c -> (newCardView c):existingCards
          Nothing -> existingCards
    existingCards = map cardView s.cards
    cardView (Entity {val: Cards.Card c}) = div ! className "card" #
      div ! className "card-content" #
        span ! className "card-title" # text c.question
