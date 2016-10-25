module Flashcards.Client.Topics
    ( Topic(..)
    , TopicId
    , getTopics
    , getTopic
    ) where


-------------------------------------------------------------------------------
import Control.Applicative ((<*>), (<$>))
import Control.Monad.Aff (attempt, Aff)
import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Data.Either (either, Either(Left))
import Data.Generic (class Generic)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid ((<>))
import Flashcards.Client.Common (Entity, Id(Id))
import Network.HTTP.Affjax (get, AJAX)
import Network.HTTP.StatusCode (StatusCode(StatusCode))
import Prelude (($), (==), map, show, pure, bind, (<<<))
-------------------------------------------------------------------------------


type TopicId = Id Topic


-------------------------------------------------------------------------------
newtype Topic = Topic {
      title :: String
    , card_count :: Int
    , last_quizzed :: String
    , avg_score :: Maybe Int
    }


derive instance genericTopic :: Generic Topic


instance decodeJsonTopic :: DecodeJson Topic where
  decodeJson json = do
    o <- decodeJson json
    title <- o .? "title"
    card_count <- o .? "card_count"
    last_quizzed <- o .? "last_quizzed"
    avg_score <- o .? "avg_score"
    pure (Topic {
        title: title
      , card_count: card_count
      , last_quizzed: last_quizzed
      , avg_score: avg_score
      })


-------------------------------------------------------------------------------
getTopics :: forall eff. Aff ( ajax :: AJAX | eff) (Either String (Array (Entity Topic)))
getTopics = do
  res <- attempt (get "/api/topics")
  let decode reply = decodeJson reply.response
  pure (either (Left <<< show) decode res)


-------------------------------------------------------------------------------
getTopic :: forall eff. TopicId -> Aff ( ajax :: AJAX | eff) (Either String (Maybe (Entity Topic)))
getTopic (Id tid) = do
  res <- attempt (get ("/api/topics/" <> show tid))
  let decode reply = if reply.status == StatusCode 404
                       then pure Nothing
                       else map Just (decodeJson reply.response)
  pure (either (Left <<< show) decode res)
