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
import Data.Lens (setJust, lens, LensP, set)
import Data.Maybe (isJust, maybe, fromMaybe, Maybe(Just, Nothing))
import Data.Monoid ((<>), mempty)
import Data.Tuple (snd, fst, Tuple(Tuple))
import Flashcards.Client.Cards (answerL, questionL)
import Flashcards.Client.Common (Entity(Entity))
import Network.HTTP.Affjax (AJAX)
import Prelude (unit, Unit, const, (<<<), pure, ($), bind, map)
import Pux (noEffects, EffModel)
import Pux.Html (label, input, form, button, span, (#), (##), div, (!), text, Html)
import Pux.Html.Attributes (id_, htmlFor, name, value, type_, disabled, className)
import Pux.Html.Events (onChange, onSubmit, onClick)
-------------------------------------------------------------------------------


data Action = RefreshTopic Topics.TopicId
            | ReceiveTopic (Either String (Maybe (Tuple (Entity Topics.Topic) (Array (Entity Cards.Card)))))
            | NewCard
            | SaveCard
            | ReceiveSaveCard (Either String Unit)
            | CancelNewCard
            | EditCardQuestion String
            | EditCardAnswer String


-------------------------------------------------------------------------------
type State = {
      topic :: Maybe (Entity Topics.Topic)
    , cards :: Array (Entity Cards.Card)
    , newCard :: Maybe Cards.Card
    }


-------------------------------------------------------------------------------
newCardL :: LensP State (Maybe Cards.Card)
newCardL = lens _.newCard (_ { newCard = _ })


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
update NewCard (s@{topic: Just (Entity e)}) = noEffects (setJust newCardL (Cards.newCard e.id) s)
update NewCard s = noEffects s -- should not be possible. noop
update SaveCard s@{newCard: Just c} = {
      state: s
    , effects: [postCard]
    }
  where
    postCard = do
      map (ReceiveSaveCard <<< map (const unit)) (Cards.createCard c)
update SaveCard s = noEffects s
update (ReceiveSaveCard (Left e)) s = noEffects s
update (ReceiveSaveCard (Right e)) s = {
      state: set newCardL Nothing s
    , effects: case s.topic of
         Just (Entity e) -> [pure (RefreshTopic e.id)]
         Nothing -> []
    }
update CancelNewCard s = noEffects s { newCard = Nothing }
update (EditCardQuestion q) (s@{newCard: Just c}) =
  noEffects (s { newCard = Just (set questionL q c) })
update (EditCardQuestion q) s = noEffects s
update (EditCardAnswer a) (s@{newCard: Just c}) =
  noEffects (s { newCard = Just (set answerL a c) })
update (EditCardAnswer a) s = noEffects s


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
      [ newCardForm c
      , div ! className "card-action" ##
          [ div ! className "btn"
                ! onClick (const CancelNewCard) #
              text "Cancel"
          , button ! type_ "submit" !
                   className "btn" #
              text "Save"
          ]
      ]
    --TODO: input field
    newCardForm (Cards.Card c) = div ! className "input-field" ##
      [input
        [ type_ "text"
        , value c.question
        , name "question"
        , id_ "question"
        , onChange (EditCardQuestion <<< _.value <<< _.target)
        ] []
      , label ! htmlFor "question" # text "Question" --TODO: is htmlFor what i want?
      --TODO: split to different input-field
      , input
        [ type_ "text"
        , value c.answer
        , name "answer"
        , id_ "answer"
        , onChange (EditCardAnswer <<< _.value <<< _.target)
        ] []
      , label ! htmlFor "answer" # text "Answer" --TODO: is htmlFor what i want?
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
