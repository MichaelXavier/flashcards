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
import Data.Lens as L
import Flashcards.Components.Topic as Topic
import Flashcards.Components.TopicTest as TopicTest
import Flashcards.Components.Topics as Topics
import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.Error.Util (exceptNoteM)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Except (ExceptT(..), runExceptT)
import DOM (DOM)
import Data.Either (Either(..))
import Data.Generic (class Generic, gShow)
import Data.Lens (Lens', lens, set, setJust)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import Data.Monoid ((<>))
import Data.Tuple (Tuple(..))
import Flashcards.Client.Cards (Card, getTopicCards)
import Flashcards.Client.Common (Entity, Id(Id), eVal)
import Flashcards.Client.Topics (Topic, TopicId, getTopic, getTopics, titleL)
import Flashcards.Util (addEffect, maybeToList)
import Network.HTTP.Affjax (AJAX)
import Prelude (class Show, bind, flip, map, pure, show, ($), (<$), (<$>), (<*), (<<<), (=<<))
import Pux (EffModel, mapEffects, mapState, noEffects, onlyEffects)
import Pux.Html (Html, div, nav, text, (!), (#), (##))
import Pux.Html.Attributes (className)
import Pux.Router (int, link, lit, end, router)
import Signal.Time (Time)
-------------------------------------------------------------------------------


data Route = TopicsR
           | TopicR TopicId
           | TopicTestR TopicId
           | NotFoundR


derive instance genericRoute :: Generic Route


instance showRoute :: Show Route where show = gShow


-------------------------------------------------------------------------------
data Action = --Routing
              PageView Route
            -- Delegated Actions
            | TopicsAction Topics.Action
            | TopicAction Topic.Action
            | TopicTestAction TopicTest.Action
            -- Data fetches
            | RefreshTopics
            | ReceiveTopics (Either String (Array (Entity Topic)))
            | RefreshTopicAndCards TopicId
            | ReceiveTopicAndCards (Either String (Tuple (Entity Topic) (Array (Entity Card))))
            --  External inputs
            | Tick Time
            -- Error handling
            | DisplayError String


-------------------------------------------------------------------------------
match :: String -> Action
match url = PageView parse
  where
    parse = fromMaybe NotFoundR (router url matcher)
    matcher = defRoute <$ end
          <|> TopicTestR <$> (lit "topics" *> topicId <* lit "test")
          <|> TopicR <$> (lit "topics" *> topicId)
          <|> TopicsR <$ lit "topics"
    defRoute = TopicsR
    topicId = Id <$> int


-------------------------------------------------------------------------------
--TODO: make these maybe, have routing load necessary data
type State = {
      currentRoute :: Route
    , topicsState :: Maybe Topics.State
    , topicState :: Maybe Topic.State
    , topicTestState :: Maybe TopicTest.State
    , error :: Maybe String
    }


-------------------------------------------------------------------------------
topicsStateL :: Lens' State (Maybe Topics.State)
topicsStateL = lens _.topicsState (_ { topicsState = _ })


-------------------------------------------------------------------------------
topicStateL :: Lens' State (Maybe Topic.State)
topicStateL = lens _.topicState (_ { topicState = _ })


-------------------------------------------------------------------------------
topicTestStateL :: Lens' State (Maybe TopicTest.State)
topicTestStateL = lens _.topicTestState (_ { topicTestState = _ })


-------------------------------------------------------------------------------
currentRouteL :: Lens' State Route
currentRouteL = lens _.currentRoute (_ { currentRoute = _ })


-------------------------------------------------------------------------------

initialState :: State
initialState = {
      currentRoute: TopicsR
    , topicsState: Nothing
    , topicState: Nothing
    , topicTestState: Nothing
    , error: Nothing
    }


-------------------------------------------------------------------------------
unloadStates :: Route -> State -> State
unloadStates TopicsR s = s { topicState = Nothing
                           , topicTestState = Nothing }
unloadStates (TopicR _) s = s { topicsState = Nothing
                              , topicTestState = Nothing}
unloadStates (TopicTestR _) s = s { topicsState = Nothing
                                  , topicState = Nothing }
unloadStates NotFoundR s = s { topicsState = Nothing
                             , topicState = Nothing
                             , topicTestState = Nothing
                             }


-------------------------------------------------------------------------------
update :: forall eff. Action -> State -> EffModel State Action (ajax :: AJAX, dom :: DOM, console :: CONSOLE | eff)
update (PageView r) s = {
    state: unloadStates r (set currentRouteL r s)
  , effects: case r of
      TopicsR -> [pure RefreshTopics]
      TopicR tid -> [pure (RefreshTopicAndCards tid)]
      (TopicTestR tid) -> [pure (RefreshTopicAndCards tid)]
      NotFoundR -> []
  }

update (TopicsAction a) s@{ topicsState: Just ts } =
  let eff = mapState (\ts' -> setJust topicsStateL ts' s) (mapEffects TopicsAction (Topics.update a ts))
  in case a of
    Topics.RefreshTopics -> addEffect (pure RefreshTopics) eff
    _ -> eff
update (TopicsAction a) s = noEffects s

update (TopicAction a) s@{ topicState: Just ts} =
  let eff = mapState (\ts' -> setJust topicStateL ts' s) (mapEffects TopicAction (Topic.update a ts))
  in case a of
    Topic.RefreshTopic tid -> addEffect (pure (RefreshTopicAndCards tid)) eff
    _ -> eff
update (TopicAction a) s = noEffects s

--TODO: route through anything that needs a clock
update (Tick t) s = noEffects s

update (TopicTestAction a) s@{ topicTestState: Just ts } =
  mapState (\ts' -> setJust topicTestStateL ts' s) (mapEffects TopicTestAction (TopicTest.update a ts))
update (TopicTestAction a) s = noEffects s

update RefreshTopics s = onlyEffects s [map ReceiveTopics getTopics]

update (ReceiveTopics (Left e)) s = update (DisplayError e) s
update (ReceiveTopics (Right topics)) s@{ currentRoute: TopicsR} =
  update (TopicsAction (Topics.ReceiveTopics topics)) s { topicsState = Just Topics.initialState}
update (ReceiveTopics _) s = noEffects s

update (RefreshTopicAndCards tid) s = onlyEffects s [map ReceiveTopicAndCards go]
  where
    go = runExceptT $ do
      t <- flip exceptNoteM "Topic not found" =<< ExceptT (getTopic tid)
      cs <- ExceptT (getTopicCards tid)
      pure (Tuple t cs)

update (ReceiveTopicAndCards (Left e)) s = update (DisplayError e) s
--TODO: should use a traversal to just set both, same goes for ticks
update (ReceiveTopicAndCards (Right (Tuple topic cards))) s@{ currentRoute: TopicR _} =
  update (TopicAction (Topic.ReceiveTopic topic cards))
         s { topicState = Just (Topic.initialState topic cards)}
update (ReceiveTopicAndCards (Right (Tuple topic cards))) s@{ currentRoute: TopicTestR _} =
  update (TopicTestAction (TopicTest.ReceiveTopic topic cards))
         s { topicTestState = Just ({topic: topic, cards: cards})}
update (ReceiveTopicAndCards (Right _)) s = noEffects s

update (DisplayError e) s = noEffects s { error = Just e }


-------------------------------------------------------------------------------
--TODO: toast errors or something
view :: State -> Html Action
view s = div ! className "container" ##
  [ navigation
  ] <> maybeToList (page s.currentRoute)
    <> maybeToList (map text s.error)
  where
    page NotFoundR = Just (text "Not found!")
    --TODO: use alt to go through loaded pages?
    page TopicsR = map (map TopicsAction <<< Topics.view) s.topicsState
    page (TopicR _) = map (map TopicAction <<< Topic.view) s.topicState
    page (TopicTestR _) = map (map TopicTestAction <<< TopicTest.view) s.topicTestState
    navigation = nav #
      div ! className "nav-wrapper" #
        div ! className "col s12" ##
        (crumbs s.currentRoute)
    crumbs NotFoundR = []
    crumbs TopicsR = [ topicsCrumb
                     ]
    crumbs (TopicR (Id tid)) = [
        topicsCrumb
      , link ("/topics/" <> tidS) ! className "breadcrumb" # text topicS
      ]
      where
        tidS = show tid
        topicS = case s.topicState of
          Just t -> L.view (eVal <<< titleL) t.topic
          Nothing -> tidS
    crumbs (TopicTestR (Id tid)) = [
        topicsCrumb
      , link ("/topics/" <> tidS) ! className "breadcrumb" # text topicS
      , link ("/topics/" <> tidS <> "/test") ! className "breadcrumb" # text "Test"
      ]
      where
        tidS = show tid
        topicS = case s.topicTestState of
          Just t -> L.view (eVal <<< titleL) t.topic
          Nothing -> tidS
    topicsCrumb = link "/topics" ! className "breadcrumb" # text "Topics"
