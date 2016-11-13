module Flashcards.Components.ExpansionPanel
    ( State(..)
    , Action(..)
    , update
    , view
    ) where


-------------------------------------------------------------------------------
import Control.Monad.Eff.JQuery as JQuery
import Control.Monad.Eff.Class (liftEff)
import DOM (DOM)
import Data.Monoid ((<>))
import Flashcards.Util (slideUp, slideDown)
import Prelude (pure, (=<<), ($), bind, const, otherwise)
import Pux (noEffects, EffModel)
import Pux.Html (i, text, (#), (##), div, (!), Html)
import Pux.Html.Attributes (id_, className)
import Pux.Html.Events (onClick)
-------------------------------------------------------------------------------

--TODO: make this stateless

type State = {
      expanded :: Boolean
    , id       :: String
    }


data Action = Expand
            | Collapse
            | Nop


update :: forall eff. Action -> State -> EffModel State Action (dom :: DOM | eff)
update Expand s = {
      state: s { expanded = true }
    , effects: [liftEff $ do
        els <- JQuery.find ".expander-reveal" =<< JQuery.select ("#" <> s.id)
        slideDown els {duration: 350, easing: "easeOutQuart", queue: false}
        pure Nop
      ]
    }
update Collapse s = {
      state: s { expanded = false }
    , effects: [liftEff $ do
        els <- JQuery.find ".expander-reveal" =<< JQuery.select ("#" <> s.id)
        slideUp els {duration: 350, easing: "easeOutQuart", queue: false}
        pure Nop
      ]
    }
update Nop s = noEffects s


--TODO: should we take html for show, hide, content
--TODO: css
-- If you make simple components just a view function, you don't need an update function for them
-- https://groups.google.com/forum/#!topic/elm-discuss/_cfOu88oCx4 suggests keeping your app as one component as long as possible, return triples from page updates
-- this *sounds* promising http://package.elm-lang.org/packages/debois/elm-parts/latest/
view :: State -> Html Action -> Html Action -> Html Action -> Html Action
view s expandLabel collapseLabel content = div ! className "expander" ! id_ s.id ##
  [ div ! className showClass ! onClick (const Expand) ##
      [ div ! className "col s11" # expandLabel
      , i ! className "material-icons col s1" # text "keyboard_arrow_down"
      ]
  , div ! className collapseClass ! onClick (const Collapse) ##
      [ div ! className "col s11" # collapseLabel
      , i ! className "material-icons col s1" # text "keyboard_arrow_up"
      ]
  , div ! className "expander-reveal row" # content
  ]
  where
    showClass
      | s.expanded = "expand row hide"
      | otherwise = "expand row"
    collapseClass
      | s.expanded = "expand row"
      | otherwise = "expand row hide"
