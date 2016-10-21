module Flashcards.Client.Topics
    ( Topic(..)
    , TopicId(..)
    , getTopics
    ) where


-------------------------------------------------------------------------------
import Control.Applicative ((<*>), (<$>))
import Control.Monad.Aff (attempt, Aff)
import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Data.Either (either, Either(Left))
import Data.Eq (class Eq)
import Data.Generic (gEq, gCompare, class Generic)
import Data.Maybe (Maybe)
import Data.Ord (class Ord)
import Network.HTTP.Affjax (get, AJAX)
import Prelude (map, show, pure, bind, (<<<))
-------------------------------------------------------------------------------


newtype TopicId = TopicId Int


derive instance genericTopicId :: Generic TopicId


instance decodeJsonTopicId :: DecodeJson TopicId where
  decodeJson = map TopicId <<< decodeJson


instance eqTopic :: Eq TopicId where
  eq = gEq


instance ordTopic :: Ord TopicId where
  compare = gCompare


-------------------------------------------------------------------------------
newtype Topic = Topic {
      id :: TopicId
    , title :: String
    , card_count :: Int
    , last_quizzed :: String
    , avg_score :: Maybe Int
    }


derive instance genericTopic :: Generic Topic


instance decodeJsonTopic :: DecodeJson Topic where
  decodeJson json = do
    o <- decodeJson json
    id <- o .? "id"
    title <- o .? "title"
    card_count <- o .? "card_count"
    last_quizzed <- o .? "last_quizzed"
    avg_score <- o .? "avg_score"
    pure (Topic {
        id: id
      , title: title
      , card_count: card_count
      , last_quizzed: last_quizzed
      , avg_score: avg_score
      })


-------------------------------------------------------------------------------
getTopics :: forall eff. Aff ( ajax :: AJAX | eff) (Either String (Array Topic))
getTopics = do
  res <- attempt (get "/topics")
  let decode reply = decodeJson reply.response
  pure (either (Left <<< show)
               decode
               res)
