module Flashcards.Components.Topics
    ( State(..)
    , TopicsMap
    , initialState
    , Action(..)
    , update
    , view
    -- * Exported for test
    , Marked(..)
    , hlText'
    ) where


-------------------------------------------------------------------------------
import Data.Array as A
import Data.Map as M
import Data.String as S
import Flashcards.Client.Topics as Topics
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Monoid ((<>), mempty)
import Data.String (toLower, Pattern(Pattern))
import Data.Tuple (Tuple(Tuple))
import Flashcards.Client.Common (Entity(Entity), Id(Id))
import Flashcards.Util (containsCI, splitAt)
import Network.HTTP.Affjax (AJAX)
import Prelude (class Show, show, map, (<<<))
import Pux (noEffects, EffModel)
import Pux.Html (mark, option, select, input, li, ul, Html, div, text, span, (##), (!), (#))
import Pux.Html.Attributes (type_, id_, placeholder, className)
import Pux.Html.Events (onKeyUp)
import Pux.Router (link)
-------------------------------------------------------------------------------


type TopicsMap = M.Map (Id Topics.Topic) (Entity Topics.Topic)


-------------------------------------------------------------------------------
type State = {
      topics :: TopicsMap
    , filterText :: String
    }


-------------------------------------------------------------------------------
initialState :: State
initialState = { topics: mempty
               , filterText: mempty
               }


-------------------------------------------------------------------------------
data Action = RefreshTopics
            | ReceiveTopics (Array (Entity Topics.Topic))
            | FilterTopics String


-------------------------------------------------------------------------------
update :: forall eff. Action -> State -> EffModel State Action (ajax :: AJAX | eff)
-- let parent handle it
update RefreshTopics s = noEffects s
update (ReceiveTopics ts) s = noEffects s {
      topics = mapTopics ts
    }
update (FilterTopics f) s = noEffects s { filterText = f }


-------------------------------------------------------------------------------
mapTopics :: Array (Entity Topics.Topic) -> TopicsMap
mapTopics = M.fromFoldable <<< map toPair
  where
    toPair topic@(Entity { id: i }) = Tuple i topic


-------------------------------------------------------------------------------
filterTopics :: String -> Array (Entity Topics.Topic) -> Array (Entity Topics.Topic)
filterTopics "" ts = ts
filterTopics srch ts = A.filter match ts
  where
    match (Entity {val: Topics.Topic t}) = containsCI srch t.title


-------------------------------------------------------------------------------
data Marked = Marked String
            | Unmarked String


instance showMarked :: Show Marked where
  show (Unmarked s) = s
  show (Marked s) = "<mark>" <> s <> "</mark>"


-------------------------------------------------------------------------------
--TODO: extract
-- | Highlight text with the given search using the mark tag
hlText :: forall a. String -> String -> Array (Html a)
hlText srch = map render <<< hlText' srch
  where
    render (Unmarked s) = text s
    render (Marked s) = mark # text s


-------------------------------------------------------------------------------
hlText' :: String -> String -> Array Marked
hlText' "" full = [Unmarked full]
hlText' srch full = go [] full
  where
    srchLen = S.length srch
    srchL = Pattern (toLower srch)
    go acc "" = acc
    go acc s = let mIdx = S.indexOf srchL sL
               in case mIdx of
                 Nothing -> acc <> [Unmarked s]
                 Just idx -> case splitAt idx s of
                   Nothing -> acc <> [Unmarked s]
                   Just (Tuple nonMatch matchStart) -> let match = S.take srchLen matchStart
                                                           rest = S.drop srchLen matchStart
                                                           acc' = acc <> [Unmarked nonMatch, Marked match]
                     in go acc' rest
      where
        sL = toLower s


-------------------------------------------------------------------------------
--TODO: structure, filters
--TODO: extract some boilerplate for html structure
view :: State -> Html Action
view s = div ! className "container" ##
  [ topicFiltersView
  , topicsView
  ]
  where
    topics = filterTopics s.filterText (A.fromFoldable (M.values s.topics))
    topicFiltersView = div ! className "row topics-filters" #
      div ! className "col s12" ##
        [ searchField
        , sortField
        ]
    searchField = div ! className "input-field col" #
      input
        [ placeholder "Search"
        , id_ "search"
        , type_ "search"
        , onKeyUp (\e -> FilterTopics e.target.value)
        ]
        []
    sortField = div ! className "input-field col" #
      select ##
        [ option # text "Name Ascending"
        , option # text "Name Descending"

        , option # text "Created Ascending"
        , option # text "Created Descending"

        , option # text "Most Recently Attempted"
        , option # text "Least Recently Attempted"
        ]
    topicsView = div ! className "row topics" #
      div ! className "card-grid col s12" ##
        map topicView topics
    topicView (Entity {id: tid, val: Topics.Topic t}) = div ! className "card s12 topic" #
      div ! className "card-content" ##
        [ span ! className "card-title" #
            link ("/topics/" <> tidS) ## hlText s.filterText t.title
        , ul ##
            [ li # text (show t.card_count <> " Cards")
            , li # text ("Last quizzed: " <> t.last_quizzed)
            , li # text ("Avg score:  " <> maybe "N/A" show t.avg_score)
            ]
        ]
        where
          tidS = case tid of
            Id x -> show x
