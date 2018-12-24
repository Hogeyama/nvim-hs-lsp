
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# LANGUAGE AllowAmbiguousTypes  #-}

{-# OPTIONS_GHC -Wall                   #-}
{-# OPTIONS_GHC -Wno-orphans            #-}

module Neovim.LSP.Protocol.Type.Record
  ( Option(..)
  , (:|:)(..), pattern L, pattern R
  , Nullable
  , FieldJSON
  , Record(..)
  , __

  , Enum'(..)
  , mkEnum
  , mkEnum'
  , mkEnum''
  , EnumAs(..)
  , EnumAsDef(..)
  , mkEnumAs
  , MatchEnum(..)
  , matchEnum
  , caseOfEnum
  , matchEnumAs
  , caseOfEnumAs
  ) where

import           RIO
import qualified RIO.HashMap             as HM
import qualified RIO.Map                 as M

import           Data.Aeson              hiding (KeyValue, Object)
import qualified Data.Aeson.Types        as J
import           Data.Extensible.Rexport
import           GHC.Generics            (Generic, Generic1)
import           GHC.TypeLits            (Symbol, KnownSymbol, symbolVal)
import           GHC.OverloadedLabels
import           Unsafe.Coerce           (unsafeCoerce)

import           Neovim                  (NvimObject (..))
import qualified Neovim                  as N (Object (..))

-------------------------------------------------------------------------------
-- Some data types
-------------------------------------------------------------------------------

newtype Record (xs :: [Assoc Symbol *]) = Record { fields :: OrigRecord xs } deriving (Generic)
deriving instance (Forall (Instance1 NFData (Field Identity)) xs) => NFData (Record xs)
deriving instance (Forall (Instance1 Show (Field Identity)) xs) => Show (Record xs)
deriving instance (Forall (Instance1 Eq   (Field Identity)) xs) => Eq   (Record xs)
deriving instance ((Forall (Instance1 Eq   (Field Identity)) xs),
                   (Forall (Instance1 Ord  (Field Identity)) xs)) => Ord  (Record xs)

type LensLike  f s t a b = (a -> f b) -> s -> f t

type LensLike' f s a = LensLike f s s a a
-- Requirements for this instance
-- + Instance head is less general than `p rep (f rep') -> p s (f t)`
--   which is the head of the instance defined in 'Data.Extensible.Label'
-- + Can be used with '(.~) :: ASetter s t a b -> b -> s -> t' without type annotation
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

__ :: forall k v xs f. (Functor f, Associate k v xs) => Proxy k -> LensLike' f (Record xs) v
__ p = unsafeCoerce (itemAssoc p :: LensLike' f (OrigRecord xs) v)

type Nullable = Maybe

-- | Optional field of JSON.
--
-- If a value of type @Record xs@ has 'None' in a field
-- it will be omit with its key when converted by 'toJSON':
--
-- >>> :set -XDataKinds -XTypeOperators -XOverloadedStrings -XOverloadedLabels
-- >>> import RIO
-- >>> import qualified Data.ByteString.Lazy.Char8 as BL
-- >>> let recordMay = Record $ Nothing =<: nil :: Record '["id" >: Maybe  Char]
-- >>> let recordOpt = Record $ None    =<: nil :: Record '["id" >: Option Char]
-- >>> BL.putStrLn $ encode $ toJSON recordMay
-- {"id":null}
-- >>> BL.putStrLn $ encode $ toJSON recordOpt
-- {}
--
-- Inversely,
--
-- >>> decode "{\"id\":null}" :: Maybe (Record '["id" >: Maybe  Int])
-- Just (Record {fields = id @= Nothing <: nil})
-- >>> decode "{}" :: Maybe (Record '["id" >: Maybe  Int])
-- Nothing
-- >>> decode "{\"id\":null}" :: Maybe (Record '["id" >: Option Int])
-- Just (Record {fields = id @= None <: nil})
-- >>> decode "{}" :: Maybe (Record '["id" >: Option Int])
-- Just (Record {fields = id @= None <: nil})
--
-- If you want a both nullable and ommitable field, use '(Option (Maybe a))':
--
-- >>> type Rec = Record '["id" >: Option (Maybe Int)]
-- >>> decode "{\"id\":null}" :: Maybe Rec
-- Just (Record {fields = id @= Some Nothing <: nil})
-- >>> decode "{}" :: Maybe Rec
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
    -- We does not need this case in fact, but some language servers are wrongly
    -- implemented around this case. e.g., rls returns something like this
    -- in response to a hover request:
    --   '{"result":{"contents":[{"language":"rust","value":"&str"}],"range":null}}'
    -- However, 'range' cannot be 'null' here.
    Just Null -> Some <$> parseJSON Null <|> return None
    Just x    -> Some <$> parseJSON x
    Nothing   -> return None
instance FromJSON a => FieldFromJSON a where
  lookupD k v = case HM.lookup (fromString k) v of
    Just x  -> parseJSON x
    Nothing -> fail $ "Missing key: " ++ k

instance Forall (KeyValue KnownSymbol FieldFromJSON) xs => FromJSON (Record xs) where
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

instance Forall (KeyValue KnownSymbol FieldToJSON) xs => ToJSON (Record xs) where
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

-- TODO implement fromObject
class FieldNvimObject a where
  toObject' :: a -> Maybe N.Object
instance {-# OVERLAPPING #-} NvimObject o => FieldNvimObject (Option o) where
  toObject' None     = Nothing
  toObject' (Some x) = Just $ toObject x
instance NvimObject o => FieldNvimObject o where
  toObject' = Just . toObject

instance (NFData (Record xs), Forall (KeyValue KnownSymbol FieldNvimObject) xs)
          => NvimObject (Record xs) where
  toObject (Record xs) = N.ObjectMap $ hfoldlWithIndexFor
    (Proxy @(KeyValue KnownSymbol FieldNvimObject))
    (\_ m kv ->
      let key  = N.ObjectString $ fromString $ symbolVal $ proxyAssocKey kv
          mval = toObject' $ runIdentity $ getField kv
      in case mval of
        Nothing  -> m
        Just val -> M.insert key val m)
    M.empty
    xs

instance (NFData (OrigRecord xs), Forall (KeyValue KnownSymbol FieldNvimObject) xs)
          => NvimObject (OrigRecord xs) where
  toObject xs = N.ObjectMap $ hfoldlWithIndexFor
    (Proxy @(KeyValue KnownSymbol FieldNvimObject))
    (\_ m kv ->
      let key  = N.ObjectString $ fromString $ symbolVal $ proxyAssocKey kv
          mval = toObject' $ runIdentity $ getField kv
      in case mval of
        Nothing  -> m
        Just val -> M.insert key val m)
    M.empty
    xs

-------------------------------------------------------------------------------
-- Enum
-------------------------------------------------------------------------------

newtype Enum' (xs :: [Symbol]) = Enum' (Proxy :| xs)
deriving instance Forall (Instance1 Eq Proxy) xs => Eq (Enum' xs)
deriving instance (Forall (Instance1 Eq Proxy) xs,
                   Forall (Instance1 Ord  Proxy) xs) => Ord (Enum' xs)
deriving instance Forall (Instance1 Show Proxy) xs => Show (Enum' xs)
instance Forall KnownSymbol xs => ToJSON (Enum' xs) where
  toJSON (Enum' v) = matchWith
      (\c _ -> toJSON $ getConst c)
      (htabulateFor (Proxy @KnownSymbol) (Const . symbolVal))
      v
instance Forall KnownSymbol xs => FromJSON (Enum' xs) where
  parseJSON = withText "Enum" $ \s ->
      hfoldMapWithIndexFor @_ @xs (Proxy @KnownSymbol)
        (\i x -> if s == fromString (symbolVal x)
                 then return (remember i (mkEnum x))
                 else mempty)
        vacancy

instance Member xs k => IsLabel k (Enum' xs) where
  fromLabel = mkEnum (Proxy @k)

mkEnum :: forall x xs proxy. Member xs x => proxy x -> Enum' xs
mkEnum _ = Enum' (embed (Proxy @x))

-- -- | use with overloaded labels:
-- --
-- -- >>> mkEnum' #refactor :: CodeActionKind
-- -- Enum' (EmbedAt $(mkMembership 1) Proxy)
mkEnum' :: Member xs x => Proxy x -> Enum' xs
mkEnum' p = Enum' (embed p)

-- -- | use with type application:
-- --
-- -- >>> mkEnum'' @"refactor.inline" :: CodeActionKind
-- -- Enum' (EmbedAt $(mkMembership 3) Proxy)
mkEnum'' :: forall x xs. Member xs x => Enum' xs
mkEnum'' = Enum' (embed (Proxy @x))

-------------------------------------------------------------------------------
-- EnumAs
-------------------------------------------------------------------------------

-- TODO
-- + lspには OtherErrorCode Int みたいな例外を表現するアレがある
-- + OrdとかはJSONに基づいてやるべき
--    + でもJ.ValueはOrdじゃない

-- |
-- >>> :set -Wno-orphans
-- >>> type Sevirity = EnumAs "Sevirity" ["warning", "error"]
-- >>> :{
--  instance EnumAsDef "Sevirity" ["warning", "error"] where
--    enumAsDict _ = Const (J.Number 1)
--                <! Const (J.Number 2)
--                <! nil
-- >>> :}
--
-- >>> toJSON (#warning :: Sevirity)
-- Number 1.0
-- >>> fromJSON @Sevirity (J.Number 1)
-- Success Sevirity::warning
--
data EnumAs (name :: Symbol) (xs :: [Symbol]) = EnumAs { unEnumAs :: Proxy :| xs }
deriving instance Forall (Instance1 Eq Proxy) xs => Eq (EnumAs name xs)
deriving instance (Forall (Instance1 Eq Proxy) xs,
                   Forall (Instance1 Ord  Proxy) xs) => Ord (EnumAs name xs)
instance (KnownSymbol name, Forall KnownSymbol xs) => Show (EnumAs name xs) where
  show (EnumAs v) = matchWith
      (\c _ -> symbolVal (Proxy @name) <> "::" <> getConst c)
      (htabulateFor (Proxy @KnownSymbol) (Const . symbolVal))
      v

class EnumAsDef (name :: Symbol) (xs :: [Symbol]) | name -> xs where
  enumAsDict :: Proxy name -> Const J.Value :* xs

instance (EnumAsDef name xs, Forall KnownSymbol xs) => ToJSON (EnumAs name xs) where
  toJSON (EnumAs v) = matchWith
      (\c _ -> getConst c)
      (htabulateFor (Proxy @KnownSymbol) $ \i -> hlookup i (enumAsDict (Proxy @name)))
      v
instance (EnumAsDef name xs, Forall KnownSymbol xs) => FromJSON (EnumAs name xs) where
  parseJSON v =
      hfoldMapWithIndexFor @_ @xs (Proxy @KnownSymbol)
        (\i x -> if v == getConst (hlookup i (enumAsDict (Proxy @name)))
                 then return (remember i (mkEnumAs x))
                 else mempty)
        vacancy

instance (EnumAsDef name xs, Member xs k) => IsLabel k (EnumAs name xs) where
  fromLabel = mkEnumAs (Proxy @k)

mkEnumAs :: forall name x xs proxy. Member xs x => proxy x -> EnumAs name xs
mkEnumAs _ = EnumAs (embed (Proxy @x))

-------------------------------------------------------------------------------
-- Matcher for Enum
-------------------------------------------------------------------------------

newtype MatchEnum a x where
  MatchEnum :: forall x a. a -> MatchEnum a x

matchEnum :: (MatchEnum a :* xs) -> Enum' xs -> a
matchEnum matcher (Enum' e) = match (hmap f matcher) e
  where f (MatchEnum a) = Match (const a)

caseOfEnum :: Enum' xs -> (MatchEnum a :* xs) -> a
caseOfEnum = flip matchEnum

matchEnumAs :: (MatchEnum a :* xs) -> EnumAs name xs -> a
matchEnumAs matcher (EnumAs e) = match (hmap f matcher) e
  where f (MatchEnum a) = Match (const a)

caseOfEnumAs :: EnumAs name xs -> (MatchEnum a :* xs) -> a
caseOfEnumAs = flip matchEnumAs

-------------------------------------------------------------------------------
-- class用意するくらいならもはやあれでは
-------------------------------------------------------------------------------




