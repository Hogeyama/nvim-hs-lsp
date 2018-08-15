
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall             #-}
{-# OPTIONS_GHC -Wno-orphans      #-}

module Neovim.LSP.Protocol.Type.Instance
  ( Option(..)
  , (:|:)(..), pattern L, pattern R
  , Nullable
  , FieldJSON
  ) where

import           RIO

import           Control.Applicative   ((<|>))
import           Data.Aeson            hiding (KeyValue, Object)
import qualified Data.Aeson.Types      as J
import           Data.Extensible       hiding (Nullable)
import           Data.Functor.Identity (Identity (..))
import qualified Data.HashMap.Strict   as HM
import qualified Data.Map              as M
import           Data.Proxy
import           Data.String           (fromString)
import           GHC.Generics          (Generic)
import           GHC.TypeLits          (KnownSymbol, symbolVal)

import           Neovim                (NvimObject (..))
import qualified Neovim                as N (Object (..))

-------------------------------------------------------------------------------
-- Some data types
-------------------------------------------------------------------------------

type Nullable = Maybe

-- | Optional field of JSON.
--
-- If a value of type @Record xs@ has 'None' in a field
-- it will be omit with its key when converted by 'toJSON':
--
-- >>> :set -XDataKinds -XTypeOperators -XOverloadedStrings -XOverloadedLabels
-- >>> import qualified Data.ByteString.Lazy.Char8 as BL
-- >>> let recordMay = #id @= Nothing <! nil :: Record '["id" >: Maybe  Char]
-- >>> let recordOpt = #id @= None    <! nil :: Record '["id" >: Option Char]
-- >>> BL.putStrLn $ encode $ toJSON recordMay
-- {"id":null}
-- >>> BL.putStrLn $ encode $ toJSON recordOpt
-- {}
--
-- Inversely,
--
-- >>> decode "{\"id\":null}" :: Maybe (Record '["id" >: Maybe  Int])
-- Just (id @= Nothing <: nil)
-- >>> decode "{}" :: Maybe (Record '["id" >: Maybe  Int])
-- Nothing
-- >>> decode "{\"id\":null}" :: Maybe (Record '["id" >: Option Int])
-- Just (id @= None <: nil)
-- >>> decode "{}" :: Maybe (Record '["id" >: Option Int])
-- Just (id @= None <: nil)
--
-- If you want a both nullable and ommitable field, use '(Option (Maybe a))':
--
-- >>> type Rec = Record '["id" >: Option (Maybe Int)]
-- >>> decode "{\"id\":null}" :: Maybe Rec
-- Just (id @= Some Nothing <: nil)
-- >>> decode "{}" :: Maybe Rec
-- Just (id @= None <: nil)
--
data Option a = Some a | None
  deriving (Show, Eq, Ord, Generic, NFData, Functor)

type family IsOptional a :: Bool where
  IsOptional (Option a) = 'True
  IsOptional a          = 'False

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
instance (FromJSON a, FromJSON b) => FromJSON (a :|: b) where
  parseJSON o =  Sum . Left  <$> parseJSON o
             <|> Sum . Right <$> parseJSON o
instance (ToJSON a, ToJSON b) => ToJSON (a :|: b) where
  toJSON (Sum (Left  o)) = toJSON o
  toJSON (Sum (Right o)) = toJSON o

{-# COMPLETE L, R #-}
pattern L :: a -> a :|: b
pattern L a = Sum (Left a)
pattern R :: b -> a :|: b
pattern R b = Sum (Right b)

--data (:|:) a b = L a | R b
--  deriving (Show, Eq, Ord, Generic, NFData)
--instance (FromJSON a, FromJSON b) => FromJSON (a :|: b) where
--  parseJSON o =  L <$> parseJSON o
--             <|> R <$> parseJSON o
--instance (ToJSON a, ToJSON b) => ToJSON (a :|: b) where
--  toJSON (L o) = toJSON o
--  toJSON (R o) = toJSON o
infixr 4 :|:

-------------------------------------------------------------------------------
-- JSON
-------------------------------------------------------------------------------

type FieldJSON a = (FieldToJSON a, FieldFromJSON a)

-- From JSON
------------

class FieldFromJSON' (b :: Bool) (a :: *) where
  lookupD :: Proxy b -> String -> J.Object -> J.Parser a
instance FromJSON a => FieldFromJSON' 'True (Option a) where
  lookupD _ k v = case HM.lookup (fromString k) v of
    Just Null -> Some <$> parseJSON Null <|> return None
    -- We does not need this case in fact, but some language servers are wrongly
    -- implemented around this case. e.g., rls returns something like this
    -- in response to a hover request:
    --   '{"result":{"contents":[{"language":"rust","value":"&str"}],"range":null}}'
    -- However, 'range' cannot be 'null' here.
    Just x    -> Some <$> parseJSON x
    Nothing   -> return None
instance FromJSON a => FieldFromJSON' 'False a where
  lookupD _ k v = case HM.lookup (fromString k) v of
    Just x  -> parseJSON x
    Nothing -> fail $ "Missing key: " ++ k
class    (FieldFromJSON' (IsOptional a) a) => FieldFromJSON a
instance (FieldFromJSON' (IsOptional a) a) => FieldFromJSON a

instance Forall (KeyValue KnownSymbol FieldFromJSON) xs => FromJSON (Record xs) where
  parseJSON = withObject "Object" $ \v ->
    hgenerateFor (Proxy @(KeyValue KnownSymbol FieldFromJSON))
    $ \(m :: Membership xs x) ->
            let k = symbolVal (proxyAssocKey m)
                z = lookupD (Proxy @(IsOptional (AssocValue x))) k v
            in  Field . Identity <$> z

-- To JSON
----------

class FieldToJSON' (b :: Bool) (a :: *) where
  toJSON' :: Proxy b -> a -> Maybe Value
instance ToJSON a => FieldToJSON' 'True (Option a) where
  toJSON' _ (Some x) = Just (toJSON x)
  toJSON' _ None     = Nothing
instance ToJSON a => FieldToJSON' 'False a where
  toJSON' _ = Just . toJSON
class    (FieldToJSON' (IsOptional a) a) => FieldToJSON a
instance (FieldToJSON' (IsOptional a) a) => FieldToJSON a
toJSON_ :: forall a. FieldToJSON a => a -> Maybe Value
toJSON_ = toJSON' (Proxy @(IsOptional a))

instance Forall (KeyValue KnownSymbol FieldToJSON) xs => ToJSON (Record xs) where
  toJSON xs = J.Object $ hfoldlWithIndexFor
    (Proxy @(KeyValue KnownSymbol FieldToJSON))
    (\_ m kv ->
      let key  = fromString $ symbolVal $ proxyAssocKey kv
          mval  = toJSON_ $ runIdentity $ getField kv
      in case mval of
        Nothing  -> m
        Just val -> HM.insert key val m)
    HM.empty
    xs

-------------------------------------------------------------------------------
-- NvimObject
-------------------------------------------------------------------------------

-- TODO implement fromObject
class FieldNvimObject' b a where
  toObject' :: Proxy b -> a -> Maybe N.Object
instance NvimObject o => FieldNvimObject' 'False o where
  toObject' _ = Just . toObject
instance NvimObject o => FieldNvimObject' 'True (Option o) where
  toObject' _ None     = Nothing
  toObject' _ (Some x) = Just $ toObject x
class    (FieldNvimObject' (IsOptional a) a) => FieldNvimObject a
instance (FieldNvimObject' (IsOptional a) a) => FieldNvimObject a
toObject_ :: forall a. FieldNvimObject a => a -> Maybe N.Object
toObject_ = toObject' (Proxy @(IsOptional a))

instance (NFData (Record xs), Forall (KeyValue KnownSymbol FieldNvimObject) xs)
          => NvimObject (Record xs) where
  toObject xs = N.ObjectMap $ hfoldlWithIndexFor
    (Proxy @(KeyValue KnownSymbol FieldNvimObject))
    (\_ m kv ->
      let key  = N.ObjectString $ fromString $ symbolVal $ proxyAssocKey kv
          mval = toObject_ $ runIdentity $ getField kv
      in case mval of
        Nothing  -> m
        Just val -> M.insert key val m)
    M.empty
    xs

