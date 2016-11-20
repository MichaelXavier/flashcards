module Flashcards.Client.Common
    ( Entity(..)
    , Id(..)
    , eId
    , eVal
    ) where


-------------------------------------------------------------------------------
import Data.Argonaut ((~>), (:=), encodeJson, class EncodeJson, (.?), decodeJson, class DecodeJson)
import Data.Generic (gShow, gCompare, gEq, class Generic)
import Data.Lens (lens, LensP)
import Prelude (show, pure, map, (<<<), class Show, class Ord, class Eq, bind)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
newtype Entity a = Entity { id :: Id a, val:: a}


-------------------------------------------------------------------------------
eId :: forall a. LensP (Entity a) (Id a)
eId = lens (\(Entity e) -> e.id) (\(Entity e) x -> Entity (e { id = x}))


-------------------------------------------------------------------------------
eVal :: forall a. LensP (Entity a) a
eVal = lens (\(Entity e) -> e.val) (\(Entity e) x -> Entity (e { val = x}))


-------------------------------------------------------------------------------
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


instance encodeJsonEntity :: (EncodeJson a) => EncodeJson (Entity a) where
  encodeJson (Entity e) = "id" := e.id
                       ~> encodeJson e.val


-------------------------------------------------------------------------------
newtype Id a = Id Int


derive instance genericId :: Generic (Id a)


instance eqId :: Eq (Id a) where eq = gEq
instance ordId :: Ord (Id a) where compare = gCompare
instance showId :: Show (Id a) where
  show (Id i) = show i
instance decodeJsonId :: DecodeJson (Id a) where
  decodeJson = map Id <<< decodeJson
instance encodeJsonId :: EncodeJson (Id a) where
  encodeJson (Id i) = encodeJson i
