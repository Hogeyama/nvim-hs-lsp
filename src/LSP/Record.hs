{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}

module LSP.Record
  ( module Export
  , Record(..)
  , Option(..)
  , Nullable
  , Void
  , (:|:)(..), pattern L, pattern R
  , FieldJSON
  , type (&)
  , __
  ) where

import           RIO                  hiding (Enum, Void)
import qualified RIO.HashMap          as HM
import qualified RIO.Map              as M

import           Data.Aeson           hiding (KeyValue, Object)
import qualified Data.Aeson.Types     as J
import           Data.Extensible      as Export hiding (Nullable, Record,
                                                 record)
import qualified Data.Extensible      as E
import           Data.TypeLits
import           GHC.Generics         (Generic, Generic1)
import           GHC.OverloadedLabels
import           Unsafe.Coerce        (unsafeCoerce)

import           Neovim               (AnsiStyle, Doc, NvimObject (..))
import qualified Neovim               as N (Object (..))

-------------------------------------------------------------------------------
-- Record
-------------------------------------------------------------------------------

newtype Record (xs :: [Assoc Symbol *]) = Record { fields :: E.Record xs }
deriving instance
  Generic (Record xs)
deriving instance
  (Forall (Instance1 NFData (Field Identity)) xs) => NFData (Record xs)
deriving instance
  (Forall (Instance1 Show (Field Identity)) xs) => Show (Record xs)
deriving instance
  (Forall (Instance1 Eq   (Field Identity)) xs) => Eq   (Record xs)
deriving instance
  ((Forall (Instance1 Eq   (Field Identity)) xs),
   (Forall (Instance1 Ord  (Field Identity)) xs)) => Ord  (Record xs)

-- (<!) :: Field Identity (k >: v) -> Record xs -> Record (k >: v ': xs)
-- x <! xs = Record $ x E.<! fields xs
type LensLike  f s t a b = (a -> f b) -> s -> f t

type LensLike' f s a = LensLike f s s a a
-- Requirements for this instance
-- + Instance head is less general than `p rep (f rep') -> p s (f t)`
--   which is the head of the instance defined in 'Data.Extensible.Label'
-- + Can be used with '(.~) :: ASetter s t a b -> b -> s -> t' without type
--   annotation
--    + 'a' and 't' should be determined by 'b' and 's'
-- + Can be used with '(^.) :: s -> Getting a s a -> a' without type annotation
--    + 'a' should be determined by 's'
instance {-# OVERLAPPING #-}
    ( Functor f
    , Associate k v xs
    , t ~ Record xs
    , v ~ v'
    ) =>
    IsLabel k (LensLike f (Record xs) t v v')
  where
    fromLabel = __ (Proxy @k)

__ :: forall k v xs f. (Functor f, Associate k v xs)
   => Proxy k -> LensLike' f (Record xs) v
__ p = unsafeCoerce (itemAssoc p :: LensLike' f (E.Record xs) v)

type Nullable = Maybe

-- | Optional field of JSON.
--
-- If a value of type @Record xs@ has 'None' in a field
-- it will be omit with its key when converted by 'toJSON':
--
-- >>> :set -XDataKinds -XTypeOperators -XOverloadedStrings -XOverloadedLabels
-- >>> import RIO
-- >>> import qualified Data.ByteString.Lazy.Char8 as BL
-- >>> let recordMay = Record @'["id" >: Nullable Char] $ Nothing =<: nil
-- >>> let recordOpt = Record @'["id" >: Option Char]   $ None    =<: nil
-- >>> BL.putStrLn $ encode $ toJSON recordMay
-- {"id":null}
-- >>> BL.putStrLn $ encode $ toJSON recordOpt
-- {}
--
-- Inversely,
--
-- >>> decode @(Record '["id" >: Nullable Int]) "{\"id\":null}"
-- Just (Record {fields = id @= Nothing <: nil})
-- >>> decode "{}" :: Maybe (Record '["id" >: Nullable  Int])
-- Nothing
-- >>> decode @(Record '["id" >: Option Int]) "{\"id\":null}"
-- Just (Record {fields = id @= None <: nil})
-- >>> decode @(Record '["id" >: Option Int]) "{}"
-- Just (Record {fields = id @= None <: nil})
--
-- If you want a both nullable and ommitable field, use '(Option (Maybe a))':
--
-- >>> type Rec = Record '["id" >: Option (Maybe Int)]
-- >>> decode @Rec "{\"id\":null}"
-- Just (Record {fields = id @= Some Nothing <: nil})
-- >>> decode @Rec "{}"
-- Just (Record {fields = id @= None <: nil})
--
data Option a = Some a | None
  deriving (Show, Eq, Ord, Functor, Generic, Generic1)
instance NFData a => NFData (Option a)

-- | Sum type for record. The differnce against 'Either' is:
--
-- >>> :set -XOverloadedStrings -XTypeOperators
-- >>> encode (Left 1 :: Either Int Bool)
-- "{\"Left\":1}"
-- >>> encode (L 1 :: Int :|: Bool)
-- "1"
--
newtype (:|:) a b = Sum (Either a b)
  deriving (Show, Eq, Ord, Generic, NFData)
infixr 4 :|:
{-# COMPLETE L, R #-}
pattern L :: a -> a :|: b
pattern L a = Sum (Left a)
pattern R :: b -> a :|: b
pattern R b = Sum (Right b)
instance (FromJSON a, FromJSON b) => FromJSON (a :|: b) where
  parseJSON o = L <$> parseJSON o <|> R <$> parseJSON o
instance (ToJSON a, ToJSON b) => ToJSON (a :|: b) where
  toJSON (L o) = toJSON o
  toJSON (R o) = toJSON o

data Void
instance Eq       Void where _ == _      = True
instance Show     Void where show        = \case{}
instance ToJSON   Void where toJSON      = \case{}
instance FromJSON Void where parseJSON _ = mempty

-- Concat
type family (&) r1 r2 where
  (&) (Record xs) (Record ys) = Record (xs ++ ys)
infixr 5 &

-------------------------------------------------------------------------------
-- JSON
-------------------------------------------------------------------------------

type FieldJSON a = (FieldToJSON a, FieldFromJSON a)

-- From JSON
------------

class FieldFromJSON a where
    lookupD :: String -> J.Object -> J.Parser a
instance {-# OVERLAPPING #-} FromJSON a => FieldFromJSON (Option a) where
    lookupD k v = case HM.lookup (fromString k) v of
      -- We does not need this case in fact, but some language servers are
      -- wrongly implemented around this case. e.g., rls returns something like
      -- this in response to a hover request:
      --   '{"result":
      --      {"contents":[{"language":"rust","value":"&str"}],"range":null}}'
      -- However, 'range' cannot be 'null' here.
      Just Null -> Some <$> parseJSON Null <|> return None
      Just x    -> Some <$> parseJSON x
      Nothing   -> return None
instance FromJSON a => FieldFromJSON a where
    lookupD k v = case HM.lookup (fromString k) v of
      Just x  -> parseJSON x
      Nothing -> fail $ "Missing key: " ++ k

instance
    Forall (KeyValue KnownSymbol FieldFromJSON) xs => FromJSON (Record xs)
  where
    parseJSON = withObject "Object" $ \v -> fmap Record $
      hgenerateFor (Proxy @(KeyValue KnownSymbol FieldFromJSON)) $ \m ->
        let k = symbolVal (proxyAssocKey m)
            z = lookupD k v
        in  Field . Identity <$> z

-- To JSON
----------

class FieldToJSON (a :: *) where
    toJSON' :: a -> Maybe Value
instance {-# OVERLAPPING #-} ToJSON a => FieldToJSON (Option a) where
    toJSON' (Some x) = Just (toJSON x)
    toJSON' None     = Nothing
instance ToJSON a => FieldToJSON a where
    toJSON' = Just . toJSON

instance
    Forall (KeyValue KnownSymbol FieldToJSON) xs => ToJSON (Record xs)
  where
    toJSON (Record xs) = J.Object $ hfoldlWithIndexFor
      (Proxy @(KeyValue KnownSymbol FieldToJSON))
      (\_ m kv ->
        let key  = fromString $ symbolVal $ proxyAssocKey kv
            mval  = toJSON' $ runIdentity $ getField kv
        in case mval of
          Nothing  -> m
          Just val -> HM.insert key val m)
      HM.empty
      xs

-------------------------------------------------------------------------------
-- NvimObject
-------------------------------------------------------------------------------

class FieldNvimObject a where
    toObject' :: a -> Maybe N.Object
    lookupObject' :: String -> Map N.Object N.Object -> Either (Doc AnsiStyle) a
instance {-# OVERLAPPING #-} NvimObject o => FieldNvimObject (Option o) where
    toObject' None     = Nothing
    toObject' (Some x) = Just $ toObject x
    lookupObject' key m = case M.lookup (N.ObjectString (fromString key)) m of
      Nothing  -> Right None
      Just obj -> Some <$> fromObject obj
instance NvimObject o => FieldNvimObject o where
    toObject' = Just . toObject
    lookupObject' key m = case M.lookup (N.ObjectString (fromString key)) m of
      Nothing  -> Left (fromString $ "key " <> key <> " not found")
      Just obj -> fromObject obj

instance
    ( NFData (Record xs)
    , Forall (KeyValue KnownSymbol FieldNvimObject) xs
    ) => NvimObject (Record xs)
  where
    toObject (Record xs) = N.ObjectMap $ hfoldlWithIndexFor
      (Proxy @(KeyValue KnownSymbol FieldNvimObject))
      (\_ map' kv ->
        let key  = N.ObjectString $ fromString $ symbolVal $ proxyAssocKey kv
            mval = toObject' $ runIdentity $ getField kv
        in case mval of
          Nothing  -> map'
          Just val -> M.insert key val map')
      M.empty
      xs

    fromObject (N.ObjectMap map') = fmap Record $
      hgenerateFor (Proxy @(KeyValue KnownSymbol FieldNvimObject)) $ \m ->
        let k = symbolVal (proxyAssocKey m)
            z = lookupObject' k map'
        in  Field . Identity <$> z
    fromObject o = Left . fromString $
                    "ObjectMap is expected, but got " <> show o

