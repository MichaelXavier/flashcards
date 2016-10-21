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
import Flashcards.Components.Topics as Topics
import Control.Alt ((<|>))
import Data.Maybe (fromMaybe)
import Network.HTTP.Affjax (AJAX)
import Prelude (pure, map, (<$))
import Pux (EffModel, mapEffects, mapState)
import Pux.Html (text, Html)
import Pux.Router (lit, end, router)
-------------------------------------------------------------------------------


data Route = TopicsR
           | NotFoundR


-------------------------------------------------------------------------------
data Action = PageView Route
            | TopicsAction Topics.Action


-------------------------------------------------------------------------------
match :: String -> Action
match url = PageView parse
  where
    parse = fromMaybe NotFoundR (router url matcher)
    matcher = defRoute <$ end
          <|> TopicsR <$ lit "topics"
    defRoute = TopicsR


-------------------------------------------------------------------------------
type State = {
      currentRoute :: Route
    , topicsState :: Topics.State
    }


-------------------------------------------------------------------------------

initialState :: State
initialState = { currentRoute: TopicsR
               , topicsState: Topics.initialState
               }


-------------------------------------------------------------------------------
update :: forall eff. Action -> State -> EffModel State Action (ajax :: AJAX | eff)
update (PageView r) s = {
    state: s { currentRoute = r }
  , effects: case r of
      TopicsR -> [pure (TopicsAction Topics.RefreshTopics)]
      _ -> []
  }
update (TopicsAction a) s =
  mapState (\ts -> s { topicsState = ts }) (mapEffects TopicsAction (Topics.update a s.topicsState))


-------------------------------------------------------------------------------
--TODO: navigation
view :: State -> Html Action
view s = page s.currentRoute
  where
    page NotFoundR = text "Not found!"
    page TopicsR = map TopicsAction (Topics.view s.topicsState)
