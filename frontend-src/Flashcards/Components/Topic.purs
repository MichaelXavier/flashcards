module Flashcards.Components.Topic
    ( Action(..)
    , State(..)
    , CardState
    , initialState
    , update
    , view
    ) where


-------------------------------------------------------------------------------
import Data.Lens as L
import Flashcards.Client.Cards as Cards
import Flashcards.Client.Topics as Topics
import Flashcards.Components.DeleteConfirm as DeleteConfirm
import Flashcards.Components.ExpansionPanel as ExpansionPanel
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Except (ExceptT(ExceptT), runExceptT)
import DOM (DOM)
import Data.Array (mapWithIndex, singleton, index, (:))
import Data.Either (either, Either(Right, Left))
import Data.Lens (_Just, over, LensP, set, appendOver, setJust, lens)
import Data.Lens.Index (ix)
import Data.Maybe (isJust, maybe, fromMaybe, Maybe(Just, Nothing))
import Data.Monoid ((<>), mempty)
import Data.Tuple (snd, fst, Tuple(Tuple))
import Flashcards.Client.Cards (updateCard, answerL, questionL)
import Flashcards.Client.Common (eId, eVal, Entity(Entity))
import Flashcards.Util (effectsL)
import Network.HTTP.Affjax (AJAX)
import Prelude (show, id, unit, Unit, const, (<<<), pure, ($), bind, map)
import Pux (mapState, mapEffects, noEffects, EffModel)
import Pux.Html (Html, div, text, a, span, input, form, h4, (##), (!), (#))
import Pux.Html.Attributes (className, href, placeholder, name, value, type_, disabled)
import Pux.Html.Events (onChange, onSubmit, onClick)
-------------------------------------------------------------------------------


data Action = RefreshTopic Topics.TopicId
            | ReceiveTopic (Either String (Maybe (Tuple (Entity Topics.Topic) (Array (Entity Cards.Card)))))
            | NewCard
            | SaveCard
            | ReceiveSaveCard (Either String Unit)
            | CancelNewCard
            | EditNewCardQuestion String
            | EditNewCardAnswer String
            | StartDeletingCard (Entity Cards.Card)
            | DeleteConfirmAction DeleteConfirm.Action
            | CardNotDeleted String
            | CardDeleted
            | Nop
            | CardExpansionAction Int ExpansionPanel.Action
            | StartEditingCard Int
            | EditCardQuestion Int String
            | EditCardAnswer Int String
            | CancelEditingCard Int
            | SaveEditingCard Int


-------------------------------------------------------------------------------
type State = {
      topic :: Maybe (Entity Topics.Topic)
    , cards :: Array CardState
    , newCard :: Maybe Cards.Card
    , deletingCard :: Maybe (Entity Cards.Card)
    , deleteConfirmState :: DeleteConfirm.State
    }


-------------------------------------------------------------------------------
type CardState = {
      card :: Entity Cards.Card
    , exp :: ExpansionPanel.State
    , edit :: Maybe Cards.Card
    }


-------------------------------------------------------------------------------
expL :: LensP CardState ExpansionPanel.State
expL = lens _.exp (_ { exp = _ })


-------------------------------------------------------------------------------
cardL :: LensP CardState (Entity Cards.Card)
cardL = lens _.card (_ { card = _ })


-------------------------------------------------------------------------------
newCardL :: LensP State (Maybe Cards.Card)
newCardL = lens _.newCard (_ { newCard = _ })


-------------------------------------------------------------------------------
cardsL :: LensP State (Array CardState)
cardsL = lens _.cards (_ { cards = _ })


-------------------------------------------------------------------------------
editL :: LensP CardState (Maybe Cards.Card)
editL = lens _.edit (_ { edit = _ })


-------------------------------------------------------------------------------
deletingCardL :: LensP State (Maybe (Entity Cards.Card))
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
update :: forall eff. Action -> State -> EffModel State Action (ajax :: AJAX, dom :: DOM, console :: CONSOLE | eff)
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
                                                , cards = fromMaybe mempty (map (map newCardState <<< snd) t)
                                                }
  where
    newCardState c = { card: c
                     , exp: { expanded: false
                            , id: ("card-expand-" <> show (L.view eId c))
                            }
                     , edit: Nothing
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
update (EditNewCardQuestion q) (s@{newCard: Just c}) =
  noEffects (s { newCard = Just (set questionL q c) })
update (EditNewCardQuestion q) s = noEffects s
update (EditNewCardAnswer a) (s@{newCard: Just c}) =
  noEffects (s { newCard = Just (set answerL a c) })
update (EditNewCardAnswer a) s = noEffects s
update (StartDeletingCard c) s = {
      state: setJust deletingCardL c s
    , effects: [pure (DeleteConfirmAction DeleteConfirm.RaiseDialog)]
    }
--TODO: capture confirm action and also delete
update (DeleteConfirmAction a) s =
  mapState whatOfDeletingCard (appendOver effectsL addEffs effModel)
  where
     whatOfDeletingCard = case a of
       DeleteConfirm.CancelDelete -> set deletingCardL Nothing
       _ -> id
     --TODO: cancel delete, clear deletingCard
     effModel = mapState (\dcs -> set deleteConfirmStateL dcs s) (mapEffects DeleteConfirmAction (DeleteConfirm.update a s.deleteConfirmState))
     deleteCard = fromMaybe [] $ do
       t <- s.topic
       cardE <- s.deletingCard
       pure [map (either CardNotDeleted (const CardDeleted)) (Cards.deleteTopicCard (L.view eId t) (L.view eId cardE))]
     addEffs = case a of
        DeleteConfirm.ConfirmDelete -> deleteCard
        _ -> []
update (CardNotDeleted e) s = {
      state: s
    , effects: [do
          liftEff (log e)
          pure Nop
        ]
    }
update CardDeleted s@{ topic: Just t} = {
      state: s
    , effects: [do
          pure (RefreshTopic (L.view eId t))
        ]
    }
update CardDeleted s = noEffects s
update Nop s = noEffects s
--TODO: use a lens + At to update card state, extract this
update (CardExpansionAction idx a) s =
  case index s.cards idx of
    Just cardState -> mapState (\expState -> set (cardsL <<< ix idx <<< expL) expState s) (mapEffects (CardExpansionAction idx) (ExpansionPanel.update a cardState.exp))
    Nothing -> noEffects s
update (EditCardQuestion idx v) s = noEffects (set (cardsL <<< ix idx <<< editL <<< _Just <<< questionL) v s)
update (EditCardAnswer idx v) s = noEffects (set (cardsL <<< ix idx <<< editL <<< _Just <<< answerL) v s)
update (CancelEditingCard idx) s = noEffects (set (cardsL <<< ix idx <<< editL) Nothing s)
update (StartEditingCard idx) s = noEffects (over (cardsL <<< ix idx) prepareCardEdit s)
  where
    prepareCardEdit :: CardState -> CardState
    prepareCardEdit cs = cs { edit = Just (L.view eVal cs.card)}
update (SaveEditingCard idx) s = fromMaybe baseEffModel $ do
  cs <- index s.cards idx
  editCard <- cs.edit
  let newEnt = set eVal editCard cs.card
  pure $ baseEffModel {
    state = set (cardsL <<< ix idx <<< cardL) newEnt baseEffModel.state
  , effects = [do
      _ <- updateCard newEnt
      pure Nop
    ]
  }
  where
    baseEffModel = noEffects (set (cardsL <<< ix idx <<< editL) Nothing s)


-------------------------------------------------------------------------------
view :: State -> Html Action
view s = div ! className "container" ##
  [ topicView
  , cardsView
  , deleteDialog
  ]
  where
    deleteDialog = map DeleteConfirmAction (DeleteConfirm.view dialogContent s.deleteConfirmState)
    dialogContent = case s.deletingCard of
      Just c -> h4 # text ("Are you sure you want to delete \"" <> L.view (eVal <<< Cards.questionL) c <> "\"?")
      Nothing -> text ""
    creatingCard = isJust s.newCard
    topicView = div ! className "row topic" ##
      maybe noTopic (singleton <<< topicView') s.topic
    noTopic = []
    topicView' (Entity {val: Topics.Topic t}) = div ! className "card col s12" ##
        [ span ! className "card-title" # text t.title
        , div ! className "card-action" #
            a ! href "#" !
              disabled creatingCard !
              onClick (const NewCard) #
              text "Add Card"
        ]
    newCardView c = form ! className "card new-card s12"
                          ! onSubmit (const SaveCard) ##
      newCardForm c <> [
        div ! className "card-action" ##
          [ a ! href "#" ! onClick (const CancelNewCard) #
              text "Cancel"
          , a ! href "#" ! onClick (const SaveCard) #
              text "Save"
          ]
      ]
    cardForm editQuestionAction editAnswerAction (Cards.Card c) =
      [
        div ! className "row" #
          inputField
            [ input
              [ type_ "text"
              , value c.question
              , name "question"
              , onChange (editQuestionAction <<< _.value <<< _.target)
              , placeholder "Question"
              ] []
            ]
        , div ! className "row" #
            inputField
              [ input
                [ type_ "text"
                , value c.answer
                , name "answer"
                , onChange (editAnswerAction <<< _.value <<< _.target)
                , placeholder "Answer"
                ] []
              ]
      ]
    inputField children = div ! className "input-field col" ## children
    newCardForm = cardForm EditNewCardQuestion EditNewCardAnswer
    cardsView = div ! className "row cards" ##
      case s.newCard of
        Just c -> (newCardView c):existingCards
        Nothing -> existingCards
    existingCards :: Array (Html Action)
    existingCards = mapWithIndex cardView s.cards
    -- composing stateful components gets annoying
    cardView :: Int -> CardState -> Html Action
    --cardView idx { edit: Just editCard } = text "TODO: edit card"
    cardView idx cs = div ! className "card s12" #
      div ! className "card-content" ##
        case cs.edit of
          Nothing ->
            [ span ! className "card-title" # text (L.view questionL card)
            , map (CardExpansionAction idx) (ExpansionPanel.view
                                             cs.exp
                                             (text "Show Answer")
                                             (text "Hide Answer")
                                             answer)
            , div ! className "card-action" ##
                [ a ! href "#" ! onClick (const (StartEditingCard idx)) #
                    text "Edit"
                , a ! href "#confirm-card-delete" ! className "red-text" ! onClick (const (StartDeletingCard e)) #
                    text "Delete"
                ]
            ]
          Just c -> cardForm (EditCardQuestion idx) (EditCardAnswer idx) c <>
            [ div ! className "card-action" ##
                [ a ! href "#" ! onClick (const (CancelEditingCard idx)) #
                    text "Cancel"
                , a ! href "#" ! onClick (const (SaveEditingCard idx)) #
                    text "Save"
                ]
            ]
      where
        -- create special class for card title, color code?
        answer = span ! className "card-title" #
          text (L.view answerL card)
        e = cs.card
        card = L.view eVal e
        cid = L.view eId e


--TODO: extract?
-- also this sucks because you need to address individual cards
type TopicCardState = {
      card :: Entity Cards.Card
    , expansion :: ExpansionPanel.State
    }
