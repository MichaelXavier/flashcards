module Flashcards.Util
    ( splitAt
    , transpose
    , containsCI
    , initModal
    , openModal
    , closeModal
    , effectsL
    , stateL
    , slideUp
    , slideDown
    , SlideOptions
    , maybeToList
    , addEffect
    ) where


-------------------------------------------------------------------------------
import Data.List as L
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.JQuery (JQuery)
import DOM (DOM)
import Data.Function.Uncurried (runFn2, mkFn2, Fn2, runFn5, Fn5)
import Data.Lens (lens, Lens)
import Data.Maybe (Maybe(Nothing, Just))
import Data.String (toLower, contains, Pattern(Pattern))
import Data.Tuple (Tuple(Tuple))
import Prelude (Unit, map, (<<<), (<>))
import Pux (CoreEffects, EffModel)
-------------------------------------------------------------------------------


-- | Backported version from purescript-strings 2.0 but uses a tuple instead of an array
-- see https://github.com/purescript/purescript-strings/issues/69
splitAt :: Int -> String -> Maybe (Tuple String String)
splitAt i s = runFn5 splitAtImpl Just Nothing (mkFn2 Tuple) i s


foreign import splitAtImpl :: Fn5 ((Tuple String String) -> Maybe (Tuple String String))
                              (Maybe (Tuple String String))
                              (Fn2 String String (Tuple String String))
                              Int
                              String
                              (Maybe (Tuple String String))


-------------------------------------------------------------------------------
transpose :: forall a. Array (Array a) -> Array (Array a)
transpose = toA <<< L.transpose <<< L.fromFoldable <<< toL
  where
    toL = L.fromFoldable <<< map L.fromFoldable
    toA = L.toUnfoldable <<< map L.toUnfoldable


-------------------------------------------------------------------------------
containsCI :: String -> String -> Boolean
containsCI srch s = contains (Pattern (toLower srch)) (toLower s)


-------------------------------------------------------------------------------
foreign import initModal :: forall eff. JQuery -> Eff (dom :: DOM | eff) Unit
foreign import openModal :: forall eff. JQuery -> Eff (dom :: DOM | eff) Unit
foreign import closeModal :: forall eff. JQuery -> Eff (dom :: DOM | eff) Unit
foreign import slideUpImpl :: forall eff. Fn2 JQuery SlideOptions (Eff (dom :: DOM | eff) Unit)
foreign import slideDownImpl :: forall eff. Fn2 JQuery SlideOptions (Eff (dom :: DOM | eff) Unit)


-------------------------------------------------------------------------------
slideUp :: forall eff. JQuery -> SlideOptions -> Eff (dom :: DOM | eff) Unit
slideUp = runFn2 slideUpImpl


-------------------------------------------------------------------------------
slideDown :: forall eff. JQuery -> SlideOptions -> Eff (dom :: DOM | eff) Unit
slideDown = runFn2 slideDownImpl


-------------------------------------------------------------------------------
type SlideOptions = {
      duration :: Int
    , easing :: String
    , queue :: Boolean
    }

-------------------------------------------------------------------------------
effectsL :: forall a b r. Lens { effects :: a | r } { effects :: b | r } a b
effectsL = lens _.effects (_ { effects = _ })


-------------------------------------------------------------------------------
stateL :: forall a b r. Lens { state :: a | r } { state :: b | r } a b
stateL = lens _.state (_ { state = _ })


-------------------------------------------------------------------------------
maybeToList :: forall a. Maybe a -> Array a
maybeToList Nothing = []
maybeToList (Just a) = [a]


-------------------------------------------------------------------------------
addEffect :: forall s a e. Aff (CoreEffects e) a -> EffModel s a e -> EffModel s a e
addEffect e m = m { effects = m.effects <> [e]}
