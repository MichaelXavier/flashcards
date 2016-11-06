module Flashcards.Components.Topic
    ( Action(..)
    , State(..)
    , initialState
    , update
    , view
    ) where


-------------------------------------------------------------------------------
import Data.Lens as L
import Flashcards.Client.Cards as Cards
import Flashcards.Client.Topics as Topics
import Flashcards.Components.DeleteConfirm as DeleteConfirm
import Control.Monad.Except (ExceptT(ExceptT), runExceptT)
import DOM (DOM)
import Data.Array ((:), singleton)
import Data.Either (either, Either(Right, Left))
import Data.Lens (appendOver, setJust, lens, LensP, set)
import Data.Maybe (isJust, maybe, fromMaybe, Maybe(Just, Nothing))
import Data.Monoid ((<>), mempty)
import Data.Tuple (snd, fst, Tuple(Tuple))
import Flashcards.Client.Cards (answerL, questionL)
import Flashcards.Client.Common (eId, eVal, Entity(Entity))
import Flashcards.Util (effectsL)
import Network.HTTP.Affjax (AJAX)
import Prelude (unit, Unit, const, (<<<), pure, ($), bind, map)
import Pux (mapState, mapEffects, noEffects, EffModel)
import Pux.Html (h4, a, Html, div, text, button, span, input, form, (##), (!), (#))
import Pux.Html.Attributes (className, href, placeholder, name, value, type_, disabled)
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
            | StartDeletingCard Cards.CardId
            | DeleteConfirmAction DeleteConfirm.Action
            | CardNotDeleted String
            | CardDeleted


-------------------------------------------------------------------------------
type State = {
      topic :: Maybe (Entity Topics.Topic)
    , cards :: Array (Entity Cards.Card)
    , newCard :: Maybe Cards.Card
    , deletingCard :: Maybe Cards.CardId
    , deleteConfirmState :: DeleteConfirm.State
    }


-------------------------------------------------------------------------------
newCardL :: LensP State (Maybe Cards.Card)
newCardL = lens _.newCard (_ { newCard = _ })


-------------------------------------------------------------------------------
deletingCardL :: LensP State (Maybe Cards.CardId)
deletingCardL = lens _.deletingCard (_ { deletingCard = _ })


-------------------------------------------------------------------------------
deleteConfirmStateL :: LensP State DeleteConfirm.State
deleteConfirmStateL = lens _.deleteConfirmState (_ { deleteConfirmState = _ })


-------------------------------------------------------------------------------
initialState :: State
initialState = { topic: Nothing
               , cards: mempty
               , newCard: Nothing
               , deletingCard: Nothing
               , deleteConfirmState: DeleteConfirm.initialState "topic-delete-card"
               }


-------------------------------------------------------------------------------
update :: forall eff. Action -> State -> EffModel State Action (ajax :: AJAX, dom :: DOM | eff)
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
update (StartDeletingCard cid) s = {
      state: setJust deletingCardL cid s
    , effects: [pure (DeleteConfirmAction DeleteConfirm.RaiseDialog)]
    }
--TODO: capture confirm action and also delete
update (DeleteConfirmAction a) s =
  appendOver effectsL addEffs effModel
  where
     --TODO: cancel delete, clear deletingCard
     effModel = mapState (\dcs -> set deleteConfirmStateL dcs s) (mapEffects DeleteConfirmAction (DeleteConfirm.update a s.deleteConfirmState))
     deleteCard = fromMaybe [] $ do
       t <- s.topic
       cid <- s.deletingCard
       pure [map (either CardNotDeleted (const CardDeleted)) (Cards.deleteTopicCard (L.view eId t) cid)]
     addEffs = case a of
        DeleteConfirm.ConfirmDelete -> deleteCard
        _ -> []
update (CardNotDeleted _) s = noEffects s
update CardDeleted s@{ topic: Just t} = {
      state: s
    , effects: [pure (RefreshTopic (L.view eId t))]
    }
update CardDeleted s = noEffects s


-------------------------------------------------------------------------------
view :: State -> Html Action
view s = div ! className "container" ##
  [ topicView
  , cardsView
  , deleteDialog
  ]
  where
    deleteDialog = map DeleteConfirmAction (DeleteConfirm.view dialogContent s.deleteConfirmState)
    dialogContent = h4 # text "Are you sure about that?"
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
      newCardForm c <> [
        div ! className "card-action" ##
          [ a ! href "#" ! onClick (const CancelNewCard) #
              text "Cancel"
          , a ! href "#" ! onClick (const SaveCard) #
              text "Save"
          ]
      ]
    inputField children = div ! className "input-field" ## children
    newCardForm (Cards.Card c) = [
        inputField
          [ input
            [ type_ "text"
            , value c.question
            , name "question"
            , onChange (EditCardQuestion <<< _.value <<< _.target)
            , placeholder "Question"
            ] []
          ]
      , inputField
          [ input
            [ type_ "text"
            , value c.answer
            , name "answer"
            , onChange (EditCardAnswer <<< _.value <<< _.target)
            , placeholder "Answer"
            ] []
          ]
      ]
    cardsView = div ! className "row cards" #
      div ! className "card-grid col s12" ##
        case s.newCard of
          Just c -> (newCardView c):existingCards
          Nothing -> existingCards
    existingCards = map cardView s.cards
    cardView e = div ! className "card" #
      div ! className "card-content" ##
        [ span ! className "card-title" # text (L.view questionL card)
        , div ! className "card-action" ##
            [ a ! href "#confirm-card-delete" ! className "red-text" ! onClick (const (StartDeletingCard cid)) #
                text "Delete"
            ]
        ]
      where
        card = L.view eVal e
        cid = L.view eId e
