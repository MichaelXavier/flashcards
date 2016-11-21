module Flashcards.Client.Cards
    ( Card(..)
    , topic_idL
    , questionL
    , answerL
    , newCard
    , CardId(..)
    , getTopicCards
    , createCard
    , updateCard
    , deleteTopicCard
    ) where


-------------------------------------------------------------------------------
import Flashcards.Client.Topics as Topics
import Control.Monad.Aff (attempt, Aff)
import Data.Argonaut (jsonEmptyObject, (~>), (:=), class EncodeJson, encodeJson, (.?), decodeJson, class DecodeJson)
import Data.Bifunctor (bimap)
import Data.Either (either, Either(Left))
import Data.Generic (class Generic)
import Data.Lens (view, lens, Lens')
import Data.Monoid (mempty, (<>))
import Flashcards.Client.Common (eId, eVal, Entity, Id(Id))
import Network.HTTP.Affjax (put_, AJAX, delete_, post, get)
import Prelude (Unit, show, pure, bind, (<<<))
-------------------------------------------------------------------------------


type CardId = Id Card


-------------------------------------------------------------------------------
newtype Card = Card {
      topic_id :: Topics.TopicId
    , question :: String
    , answer :: String
    }


newCard :: Topics.TopicId -> Card
newCard tid = Card { topic_id: tid
                   , question: mempty
                   , answer: mempty
                   }


derive instance genericCard :: Generic Card


instance decodeJsonCard :: DecodeJson Card where
  decodeJson json = do
    o <- decodeJson json
    topic_id <- o .? "topic_id"
    question <- o .? "question"
    answer <- o .? "answer"
    pure (Card {
        topic_id: topic_id
      , question: question
      , answer: answer
      })


instance encodeJsonCard :: EncodeJson Card where
  encodeJson (Card c) = "topic_id" := c.topic_id
                     ~> "question" := c.question
                     ~> "answer" := c.answer
                     ~> jsonEmptyObject


-------------------------------------------------------------------------------
-- Lenses
-------------------------------------------------------------------------------
topic_idL :: Lens' Card Topics.TopicId
topic_idL = lens (\(Card c) -> c.topic_id) (\(Card c) x -> Card (c { topic_id = x}))


questionL :: Lens' Card String
questionL = lens (\(Card c) -> c.question) (\(Card c) x -> Card (c { question = x}))


answerL :: Lens' Card String
answerL = lens (\(Card c) -> c.answer) (\(Card c) x -> Card (c { answer = x}))


-------------------------------------------------------------------------------
getTopicCards :: forall eff. Topics.TopicId -> Aff ( ajax :: AJAX | eff) (Either String (Array (Entity Card)))
getTopicCards (Id tid) = do
  res <- attempt (get ("/api/topics/" <> show tid <> "/cards"))
  let decode reply = decodeJson reply.response
  pure (either (Left <<< show) decode res)


-------------------------------------------------------------------------------
createCard :: forall eff. Card -> Aff ( ajax :: AJAX | eff) (Either String (Entity Card))
createCard c@(Card card) = do
  res <- attempt (post ("/api/topics/" <> tid <> "/cards") (encodeJson c))
  let decode reply = decodeJson reply.response
  pure (either (Left <<< show) decode res)
  where
    tid = case card.topic_id of
      Id i -> show i


-------------------------------------------------------------------------------
updateCard :: forall eff. Entity Card -> Aff ( ajax :: AJAX | eff) (Either String Unit)
updateCard e = do
  res <- attempt (put_ ("/api/topics/" <> show tid <> "/cards/" <> show cid) (encodeJson e))
  pure (bimap show _.response res)
  where
    card = view eVal e
    cid = view eId e
    tid = view topic_idL card



-------------------------------------------------------------------------------
deleteTopicCard :: forall eff. Topics.TopicId -> CardId -> Aff ( ajax :: AJAX | eff) (Either String Unit)
deleteTopicCard (Id tid) (Id cid) = do
  res <- attempt (delete_ ("/api/topics/" <> show tid <> "/cards/" <> show cid))
  pure (bimap show _.response res)
