module Flashcards.Client.Cards
    ( Card(..)
    , CardId(..)
    , getTopicCards
    ) where


-------------------------------------------------------------------------------
import Flashcards.Client.Topics as Topics
import Control.Monad.Aff (attempt, Aff)
import Data.Argonaut ((.?), decodeJson, class DecodeJson)
import Data.Either (either, Either(Left))
import Data.Generic (gEq, gCompare, class Generic)
import Data.Monoid ((<>))
import Network.HTTP.Affjax (get, AJAX)
import Prelude (pure, show, class Ord, class Eq, map, (<<<), bind)
-------------------------------------------------------------------------------


newtype CardId = CardId Int


derive instance genericCardId :: Generic CardId


instance decodeJsonCardId :: DecodeJson CardId where
  decodeJson = map CardId <<< decodeJson


instance eqCard :: Eq CardId where
  eq = gEq


instance ordCard :: Ord CardId where
  compare = gCompare


-------------------------------------------------------------------------------
newtype Card = Card {
      id :: CardId
    , topic_id :: Topics.TopicId
    , question :: String
    , answer :: String
    }



derive instance genericCard :: Generic Card


instance decodeJsonCard :: DecodeJson Card where
  decodeJson json = do
    o <- decodeJson json
    id <- o .? "id"
    topic_id <- o .? "topic_id"
    question <- o .? "question"
    answer <- o .? "answer"
    pure (Card {
        id: id
      , topic_id: topic_id
      , question: question
      , answer: answer
      })


-------------------------------------------------------------------------------
getTopicCards :: forall eff. Topics.TopicId -> Aff ( ajax :: AJAX | eff) (Either String (Array Card))
getTopicCards (Topics.TopicId tid) = do
  res <- attempt (get ("/api/topics/" <> show tid <> "/cards"))
  let decode reply = decodeJson reply.response
  pure (either (Left <<< show) decode res)
