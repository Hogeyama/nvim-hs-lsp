{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE UndecidableInstances #-}

module LSP.Enum
  ( EnumN
  , EnumS
  , pattern MatchEnum
  , matchEnum
  , caseOfEnum
  ) where

import           RIO                  hiding (Enum, Void)

import           Data.Aeson           hiding (KeyValue, Object)
import           Data.Extensible      hiding (Nullable)
import qualified Data.Extensible      as E
import           Data.Monoid          (getFirst)
import           Data.TypeLits
import           GHC.OverloadedLabels

import           Neovim               (NvimObject (..))
import qualified Neovim               as N (Object (..))

-------------------------------------------------------------------------------
-- Enum
-------------------------------------------------------------------------------

newtype Enum (xs :: [Assoc Symbol k]) = Enum { unEnum :: VariantOf Proxy xs }
  deriving (Generic)
instance Eq  (Enum xs) where (==)    = (==)    `on` idOfVariant.unEnum
instance Ord (Enum xs) where compare = compare `on` idOfVariant.unEnum
instance (NFData (VariantOf Proxy xs)) => NFData (Enum xs)
instance (Forall (KeyIs KnownSymbol) xs) => Show (Enum xs) where
  show (Enum xs) = matchWithField
    (\(Const key) _ -> "#"<>key)
    (htabulateFor (Proxy @(KeyIs KnownSymbol))
      (\(_ :: Membership xs x)->
          Field (Const (symbolVal (Proxy @(AssocKey x))))))
    xs
instance Associate k v xs => IsLabel k (Enum xs) where
  fromLabel = Enum $ embedAssoc (fromLabel @k @= ())

-- Enum as Number
-----------------

type EnumN (xs :: [Assoc Symbol TInt]) = Enum xs

instance Forall (ValueIs KnownInt) xs => ToJSON (EnumN xs) where
  toJSON = toJSON . enumToInteger
instance Forall (ValueIs KnownInt) xs => FromJSON (EnumN xs) where
  parseJSON = withScientific "Enum" $ round >>> enumFromInteger >>> \case
      Nothing -> mempty
      Just x  -> return x
instance ( NFData (EnumN xs)
         , Forall (ValueIs KnownInt) xs
         ) => NvimObject (EnumN xs) where
  toObject = toObject . enumToInteger
  fromObject = \case
      N.ObjectInt  x -> fromIntegral' x
      N.ObjectUInt x -> fromIntegral' x
      o -> Left . fromString $ "ObjectInt is expected, but got " <> show o
    where
      fromIntegral' x = case enumFromInteger (fromIntegral x) of
        Nothing -> Left "Enum out of bounds"
        Just e  -> return e

-- Enum as String
-----------------

type EnumS (xs :: [Symbol]) = Enum (MakeIdentityMap xs)
type family MakeIdentityMap (xs :: [Symbol]) where
  MakeIdentityMap '[] = '[]
  MakeIdentityMap (k ': ks) = (k >: k) ': MakeIdentityMap ks

instance Forall (ValueIs KnownSymbol) xs => ToJSON (Enum xs) where
  toJSON = toJSON . enumToText
instance Forall (ValueIs KnownSymbol) xs => FromJSON (Enum xs) where
  parseJSON = withText "Enum" $ enumFromText >>> \case
      Nothing -> mempty
      Just x  -> return x
instance ( NFData (Enum xs)
         , Forall (ValueIs KnownSymbol) xs
         ) => NvimObject (Enum xs) where
  toObject = toObject . enumToText
  fromObject = \case
      N.ObjectString b -> fromBytes b
      N.ObjectBinary b -> fromBytes b
      o -> Left . fromString $ "ObjectString is expected, but got " <> show o
    where
      fromBytes b = case enumFromText (decodeUtf8Lenient b) of
        Nothing -> Left "Enum out of bounds"
        Just e  -> return e


-------------------------------------------------------------------------------
-- Matcher
-------------------------------------------------------------------------------

newtype MatchEnum a x where
  MatchEnum' :: forall x a. a -> MatchEnum a x

pattern MatchEnum :: forall k v a. a -> MatchEnum a (k >: v)
pattern MatchEnum a = MatchEnum' a

matchEnum :: (MatchEnum a :* xs) -> EnumN xs -> a
matchEnum matcher (Enum e) = match (hmap f matcher) e
  where f (MatchEnum' a) = Match (const a)

caseOfEnum :: EnumN xs -> (MatchEnum a :* xs) -> a
caseOfEnum = flip matchEnum

-- Util for this module
-----------------------

mkEnum :: forall x xs proxy. Member xs x => proxy x -> Enum xs
mkEnum _ = Enum $ embed (Field (Proxy @(AssocValue x)) :: Field Proxy x)

idOfVariant :: VariantOf h xs -> Int
idOfVariant (EmbedAt i _) = getMemberId i

enumFromInteger :: forall xs. Forall (ValueIs KnownInt) xs => Integer -> Maybe (Enum xs)
enumFromInteger n = getFirst $
    hfoldMapWithIndexFor @_ @xs (Proxy @(ValueIs KnownInt))
      (\i (x :: E.Nullable (Field Proxy) x) ->
            if n == intVal (Proxy @(AssocValue x))
            then return (remember i (mkEnum x))
            else mempty)
      vacancy

enumToInteger :: forall xs. Forall (ValueIs KnownInt) xs => Enum xs -> Integer
enumToInteger (Enum xs) = matchWithField
    (\(Const val) _ -> val)
    (htabulateFor (Proxy @(ValueIs KnownInt)) $ \(_ :: Membership xs x)->
      Field (Const (intVal (Proxy @(AssocValue x)))))
    xs

enumFromText :: forall xs. Forall (ValueIs KnownSymbol) xs => Text -> Maybe (Enum xs)
enumFromText s = getFirst $
    hfoldMapWithIndexFor @_ @xs (Proxy @(ValueIs KnownSymbol))
      (\i (x :: E.Nullable (Field Proxy) x) ->
            if s == fromString (symbolVal (Proxy @(AssocValue x)))
            then return (remember i (mkEnum x))
            else mempty)
      vacancy

enumToText :: forall xs. Forall (ValueIs KnownSymbol) xs => Enum xs -> Text
enumToText (Enum xs) = matchWithField
    (\(Const val) _ -> fromString val)
    (htabulateFor (Proxy @(ValueIs KnownSymbol)) $ \(_ :: Membership xs x)->
      Field (Const (symbolVal (Proxy @(AssocValue x)))))
    xs

