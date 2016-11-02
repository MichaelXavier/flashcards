module Flashcards.Client.Cards
    ( Card(..)
    , newCard
    , CardId(..)
    , getTopicCards
    , createCard
    ) where


-------------------------------------------------------------------------------
import Flashcards.Client.Topics as Topics
import Control.Monad.Aff (attempt, Aff)
import Data.Argonaut (jsonEmptyObject, (~>), (:=), class EncodeJson, encodeJson, (.?), decodeJson, class DecodeJson)
import Data.Either (either, Either(Left))
import Data.Generic (class Generic)
import Data.Monoid (mempty, (<>))
import Flashcards.Client.Common (Entity, Id(Id))
import Network.HTTP.Affjax (post, get, AJAX)
import Prelude (show, pure, bind, (<<<))
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
