module Flashcards.Components.App
    ( State(..)
    , initialState
    , Action(..)
    , Route(..)
    , update
    , view
    , match
    ) where


-------------------------------------------------------------------------------
import Flashcards.Components.Topic as Topic
import Flashcards.Components.Topics as Topics
import Control.Alt ((<|>))
import Control.Apply ((*>))
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Flashcards.Client.Topics (TopicId(TopicId))
import Network.HTTP.Affjax (AJAX)
import Prelude (show, (<$>), pure, map, (<$))
import Pux (EffModel, mapEffects, mapState)
import Pux.Html (Html, div, text, nav, (##), (!), (#))
import Pux.Html.Attributes (className)
import Pux.Router (int, link, lit, end, router)
-------------------------------------------------------------------------------


data Route = TopicsR
           | TopicR TopicId
           | NotFoundR


-------------------------------------------------------------------------------
data Action = PageView Route
            | TopicsAction Topics.Action
            | TopicAction Topic.Action


-------------------------------------------------------------------------------
match :: String -> Action
match url = PageView parse
  where
    parse = fromMaybe NotFoundR (router url matcher)
    matcher = defRoute <$ end
          <|> TopicR <$> (lit "topics" *> topicId)
          <|> TopicsR <$ lit "topics"
    defRoute = TopicsR
    topicId = TopicId <$> int


-------------------------------------------------------------------------------
type State = {
      currentRoute :: Route
    , topicsState :: Topics.State
    , topicState :: Topic.State
    }


-------------------------------------------------------------------------------

initialState :: State
initialState = { currentRoute: TopicsR
               , topicsState: Topics.initialState
               , topicState: Topic.initialState
               }


-------------------------------------------------------------------------------
update :: forall eff. Action -> State -> EffModel State Action (ajax :: AJAX | eff)
update (PageView r) s = {
    state: s { currentRoute = r }
  , effects: case r of
      TopicsR -> [pure (TopicsAction Topics.RefreshTopics)]
      TopicR tid -> [pure (TopicAction (Topic.RefreshTopic tid))]
      _ -> []
  }
update (TopicsAction a) s =
  mapState (\ts -> s { topicsState = ts }) (mapEffects TopicsAction (Topics.update a s.topicsState))
update (TopicAction a) s = mapState (\ts -> s { topicState = ts }) (mapEffects TopicAction (Topic.update a s.topicState))


-------------------------------------------------------------------------------
view :: State -> Html Action
view s = div ! className "container" ##
  [ navigation
  , page s.currentRoute
  ]
  where
    page NotFoundR = text "Not found!"
    page TopicsR = map TopicsAction (Topics.view s.topicsState)
    page (TopicR _) = map TopicAction (Topic.view s.topicState)
    navigation = nav #
      div ! className "nav-wrapper" #
        div ! className "col s12" ##
        (crumbs s.currentRoute)
    crumbs NotFoundR = []
    crumbs TopicsR = [ topicsCrumb
                     ]
    crumbs (TopicR (TopicId tid)) = [
        topicsCrumb
      , link ("/topics" <> tidS) ! className "breadcrumb" # text tidS
      ]
      where
        tidS = show tid
    topicsCrumb = link "/topics" ! className "breadcrumb" # text "Topics"