module Flashcards.Util
    ( splitAt
    , transpose
    ) where


-------------------------------------------------------------------------------
import Data.List as L
import Data.Function.Uncurried (mkFn2, Fn2, runFn5, Fn5)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Tuple (Tuple(Tuple))
import Prelude (map, (<<<))
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
