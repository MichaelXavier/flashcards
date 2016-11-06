module Flashcards.Components.DeleteConfirm
    ( Action(..)
    , State(..)
    , initialState
    , update
    , view
    ) where

-------------------------------------------------------------------------------
import Control.Monad.Eff.JQuery as JQ
import Control.Monad (unless)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.JQuery (JQuery)
import DOM (DOM)
import Data.Monoid ((<>))
import Flashcards.Util (initModal, openModal)
import Prelude (pure, ($), bind, const)
import Pux (noEffects, EffModel)
import Pux.Html (a, text, (#), (##), div, (!), Html)
import Pux.Html.Attributes (id_, href, className)
import Pux.Html.Events (onClick)
-------------------------------------------------------------------------------



data Action = ConfirmDelete
            | CancelDelete
            | RaiseDialog
            | Nop


--TODO: consider this being stateless, just pass in arg to view?
type State = {
      elementId :: String
    , initialized :: Boolean
      -- ^ Unique id of the element
    }


initialState :: String -> State
initialState eid = {elementId: eid, initialized: false}


-- how do i actually get a ref to this node
update :: forall eff. Action -> State -> EffModel State Action ( dom :: DOM | eff)
update RaiseDialog s = {
      state: s { initialized = true }
    , effects: [liftEff $ do
        self <- findSelf s
        unless s.initialized (initModal self)
        openModal self
        pure Nop
      ]
    }
update _ s = noEffects s


findSelf :: forall eff. State -> Eff ( dom :: DOM | eff) JQuery
findSelf s = JQ.select ("#" <> s.elementId)


view :: Html Action -> State -> Html Action
view content s = div ! className "modal" ! id_ s.elementId ##
  [ div ! className "modal-content" # content
  , div ! className "modal-footer" ##
      [ a ! href "#" ! className "modal-close" ! onClick (const CancelDelete) # text "Cancel"
      , a ! href "#" ! className "modal-close red-text" ! onClick (const ConfirmDelete) # text "Delete"
      ]
  ]
