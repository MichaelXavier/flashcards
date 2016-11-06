module Flashcards.Util
    ( splitAt
    , transpose
    , containsCI
    , initModal
    , openModal
    , closeModal
    , effectsL
    , stateL
    ) where


-------------------------------------------------------------------------------
import Data.List as L
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.JQuery (JQuery)
import DOM (DOM)
import Data.Function.Uncurried (mkFn2, Fn2, runFn5, Fn5)
import Data.Lens (lens, Lens)
import Data.Maybe (Maybe(Nothing, Just))
import Data.String (toLower, contains)
import Data.Tuple (Tuple(Tuple))
import Prelude (Unit, map, (<<<))
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
containsCI srch s = contains (toLower srch) (toLower s)


-------------------------------------------------------------------------------
foreign import initModal :: forall eff. JQuery -> Eff (dom :: DOM | eff) Unit
foreign import openModal :: forall eff. JQuery -> Eff (dom :: DOM | eff) Unit
foreign import closeModal :: forall eff. JQuery -> Eff (dom :: DOM | eff) Unit


-------------------------------------------------------------------------------
effectsL :: forall a b r. Lens { effects :: a | r } { effects :: b | r } a b
effectsL = lens _.effects (_ { effects = _ })


-------------------------------------------------------------------------------
stateL :: forall a b r. Lens { state :: a | r } { state :: b | r } a b
stateL = lens _.state (_ { state = _ })
