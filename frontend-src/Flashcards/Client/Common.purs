module Flashcards.Client.Common
    ( Entity(..)
    , Id(..)
    ) where


-------------------------------------------------------------------------------
import Data.Argonaut ((.?), decodeJson, class DecodeJson)
import Data.Generic (gShow, gCompare, gEq, class Generic)
import Prelude (pure, map, (<<<), class Show, class Ord, class Eq, bind)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
newtype Entity a = Entity { id :: Id a, val:: a}


derive instance genericEntity :: Generic a => Generic (Entity a)


instance eqEntity :: (Eq a, Generic a) => Eq (Entity a) where eq = gEq
instance ordEntity :: (Ord a, Generic a) => Ord (Entity a) where compare = gCompare
instance showEntity :: (Show a, Generic a) => Show (Entity a) where show = gShow
instance decodeJsonEntity :: (DecodeJson a) => DecodeJson (Entity a) where
  decodeJson v = do
    o <- decodeJson v
    id <- o .? "id"
    val <- decodeJson v
    pure (Entity { id: id, val: val})


-------------------------------------------------------------------------------
newtype Id a = Id Int


derive instance genericId :: Generic (Id a)


instance eqId :: Eq (Id a) where eq = gEq
instance ordId :: Ord (Id a) where compare = gCompare
instance showId :: (Show a, Generic a) => Show (Id a) where show = gShow
instance decodeJsonId :: DecodeJson (Id a) where
  decodeJson = map Id <<< decodeJson
