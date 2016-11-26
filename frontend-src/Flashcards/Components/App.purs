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
import Flashcards.Components.Topics as Topics
import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Data.Lens (lens, Lens', set)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import Data.Monoid ((<>))
import Flashcards.Client.Common (eVal, Id(Id))
import Flashcards.Client.Topics (titleL, TopicId)
import Network.HTTP.Affjax (AJAX)
import Prelude (map, pure, show, (<$), (<$>), (<<<))
import Pux (EffModel, mapEffects, mapState, noEffects)
import Pux.Html (Html, div, nav, text, (!), (#), (##))
import Pux.Html.Attributes (className)
import Pux.Router (int, link, lit, end, router)
import Signal.Time (Time)
-------------------------------------------------------------------------------


data Route = TopicsR
           | TopicR TopicId
           | NotFoundR


-------------------------------------------------------------------------------
data Action = PageView Route
            | TopicsAction Topics.Action
            | TopicAction Topic.Action
            | Tick Time


-------------------------------------------------------------------------------
match :: String -> Action
match url = PageView parse
  where
    parse = fromMaybe NotFoundR (router url matcher)
    matcher = defRoute <$ end
          <|> TopicR <$> (lit "topics" *> topicId)
          <|> TopicsR <$ lit "topics"
    defRoute = TopicsR
    topicId = Id <$> int


-------------------------------------------------------------------------------
type State = {
      currentRoute :: Route
    , topicsState :: Topics.State
    , topicState :: Topic.State
    , currentTime :: Time
    }


-------------------------------------------------------------------------------
topicsStateL :: Lens' State Topics.State
topicsStateL = lens _.topicsState (_ { topicsState = _ })


-------------------------------------------------------------------------------
topicStateL :: Lens' State Topic.State
topicStateL = lens _.topicState (_ { topicState = _ })


-------------------------------------------------------------------------------

initialState :: Time -> State
initialState now = {
      currentRoute: TopicsR
    , topicsState: Topics.initialState
    , topicState: Topic.initialState
    , currentTime: now
    }


-------------------------------------------------------------------------------
update :: forall eff. Action -> State -> EffModel State Action (ajax :: AJAX, dom :: DOM, console :: CONSOLE | eff)
update (PageView r) s = {
    state: s { currentRoute = r }
  , effects: case r of
      TopicsR -> [pure (TopicsAction Topics.RefreshTopics)]
      TopicR tid -> [pure (TopicAction (Topic.RefreshTopic tid))]
      _ -> []
  }
update (TopicsAction a) s =
  mapState (\ts -> set topicsStateL ts s) (mapEffects TopicsAction (Topics.update a s.topicsState))
update (TopicAction a) s = mapState (\ts -> set topicStateL ts s) (mapEffects TopicAction (Topic.update a s.topicState))
update (Tick t) s = noEffects s { currentTime = t }


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
    crumbs (TopicR (Id tid)) = [
        topicsCrumb
      , link ("/topics" <> tidS) ! className "breadcrumb" # text topicS
      ]
      where
        tidS = show tid
        topicS = case s.topicState.topic of
          Just t -> L.view (eVal <<< titleL) t
          Nothing -> tidS
    topicsCrumb = link "/topics" ! className "breadcrumb" # text "Topics"
