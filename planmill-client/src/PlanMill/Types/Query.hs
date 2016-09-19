{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}
module PlanMill.Types.Query (
    Query (..),
    QueryTag (..),
    SomeQuery(..),
    queryToRequest,
    queryDict,
    queryTagDict,
    ) where

import PlanMill.Internal.Prelude

import Data.Constraint
import Data.GADT.Compare  (GEq (..), defaultEq)
import Data.Type.Equality

import Data.Swagger              (NamedSchema (..), ToSchema (..))
import Numeric.Interval.NonEmpty (Interval)

import qualified Data.Aeson.Compat                    as Aeson
import qualified Data.Map                             as Map
import qualified Data.Text                            as T
import qualified Database.PostgreSQL.Simple.FromField as Postgres
import qualified Database.PostgreSQL.Simple.ToField   as Postgres

import PlanMill.Types.Me             (Me)
import PlanMill.Types.Request
       (PlanMill (..), QueryString, planMillPagedGetQs)
import PlanMill.Types.ResultInterval
       (IntervalType (..), ResultInterval (..), intervalDayToIntervalUTC,
       intervalToQueryString)
import PlanMill.Types.Timereport     (Timereports)
import PlanMill.Types.UrlPart        (UrlParts)
import PlanMill.Types.User           (User, UserId)
import PlanMill.Types.UserCapacity   (UserCapacities)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Arbitrary queries are tagged by this values
data QueryTag a where
    QueryTagMe   :: QueryTag Me
    QueryTagUser :: QueryTag User

-- | Planmill query (i.e. read-only operation).
--
-- More sophisticated then 'PlanMill' -request,
-- as it can be serialised.
data Query a where
    QueryGet         :: QueryTag a -> QueryString -> UrlParts -> Query a
    QueryPagedGet    :: QueryTag a -> QueryString -> UrlParts -> Query (Vector a)
    QueryTimereports :: Interval Day -> UserId -> Query Timereports
    QueryCapacities  :: Interval Day -> UserId -> Query UserCapacities
  deriving (Typeable)

-- | Transform Planmill query to request
queryToRequest :: Query a -> PlanMill a
queryToRequest (QueryGet _ qs ps)      = PlanMillGet qs ps
queryToRequest (QueryPagedGet _ qs ps) = PlanMillPagedGet  qs ps
queryToRequest (QueryTimereports i u) =
    planMillPagedGetQs (qs <> qs') ("timereports" :: Text)
  where
    qs :: QueryString
    qs = intervalToQueryString
        $ ResultInterval IntervalStart
        $ intervalDayToIntervalUTC i

    qs' :: QueryString
    qs' = Map.fromList [ ("person", T.pack $ show u)
          ]
queryToRequest _                       = error "queryToRequest: not -implemented"

-- | Existential 'Query'
--
-- We could use:
--
-- @
-- data Some f where Some :: f a -> Some f
-- @
--
-- But than we have problems with writing instances.
-- This special case should be good for fit for us.
data SomeQuery where SomeQuery :: Query a -> SomeQuery
deriving instance Show SomeQuery

-- Helper to make deserialisation of SomeQuery simpler
data SomeQueryTag where SomeQueryTag :: QueryTag a -> SomeQueryTag

-------------------------------------------------------------------------------
-- QueryTag instances
-------------------------------------------------------------------------------

-- All tags are equal to itself
instance Eq (QueryTag a) where _ == _ = True
instance Ord (QueryTag a) where compare _ _ = EQ

instance GEq QueryTag where
    geq QueryTagMe QueryTagMe     = Just Refl
    geq QueryTagUser QueryTagUser = Just Refl
    geq _ _                       = Nothing

instance Eq SomeQueryTag where
    SomeQueryTag t == SomeQueryTag t' = defaultEq t t'

instance Show (QueryTag a) where
    showsPrec _ r = case r of
        QueryTagMe   -> showString "QueryTagMe"
        QueryTagUser -> showString "QueryTagUser"

instance Hashable (QueryTag a) where
    hashWithSalt salt QueryTagMe   = salt `hashWithSalt` (0 :: Int)
    hashWithSalt salt QueryTagUser = salt `hashWithSalt` (1 :: Int)

instance NFData (QueryTag a) where
    rnf x = x `seq` ()

instance ToJSON (QueryTag a) where
    toJSON QueryTagMe   = String "me"
    toJSON QueryTagUser = String "user"

instance FromJSON SomeQueryTag where
    parseJSON (String "me")   = pure $ SomeQueryTag QueryTagMe
    parseJSON (String "user") = pure $ SomeQueryTag QueryTagUser
    parseJSON (String t)      = fail $ "Invalid tag: " ++ show t
    parseJSON v               = typeMismatch "QueryTag" v

-------------------------------------------------------------------------------
-- Query instances
-------------------------------------------------------------------------------

deriving instance Eq (Query a)
-- deriving instance Ord (Query a)

instance GEq Query where
    geq (QueryGet t q p) (QueryGet t' q' p') = do
        Refl <- geq t t'
        guard (q == q' && p == p')
        pure Refl
    geq (QueryPagedGet t q p) (QueryPagedGet t' q' p') = do
        Refl <- geq t t'
        guard (q == q' && p == p')
        pure Refl
    geq (QueryTimereports i u) (QueryTimereports i' u')
        | i == i' && u == u' = Just Refl
    geq (QueryCapacities i u) (QueryCapacities i' u')
        | i == i' && u == u' = Just Refl
    geq _ _ = Nothing

instance Show (Query a) where
    showsPrec d r = showParen (d > 10) $ case r of
        QueryGet t q p -> showString "QueryGet "
            . showsPrec 11 t . showString " "
            . showsPrec 11 q . showString " "
            . showsPrec 11 p
        QueryPagedGet t q p -> showString "QueryPagedGet "
            . showsPrec 11 t . showString " "
            . showsPrec 11 q . showString " "
            . showsPrec 11 p
        QueryTimereports i u -> showString "QueryTimereports "
            . showsPrec 11 i . showString " "
            . showsPrec 11 u
        QueryCapacities i u -> showString "QueryCapacities "
            . showsPrec 11 i . showString " "
            . showsPrec 11 u

instance Hashable (Query a) where
    hashWithSalt salt (QueryGet t q p) = salt
        `hashWithSalt` (0 :: Int)
        `hashWithSalt` t
        `hashWithSalt` q
        `hashWithSalt` p
    hashWithSalt salt (QueryPagedGet t q p) = salt
        `hashWithSalt` (1 :: Int)
        `hashWithSalt` t
        `hashWithSalt` q
        `hashWithSalt` p
    hashWithSalt salt (QueryTimereports i u) = salt
        `hashWithSalt` (2 :: Int)
        `hashWithSalt` i
        `hashWithSalt` u
    hashWithSalt salt (QueryCapacities i u) = salt
        `hashWithSalt` (3 :: Int)
        `hashWithSalt` i
        `hashWithSalt` u

instance NFData (Query a) where
    rnf (QueryGet t q p)       = rnf t `seq` rnf q `seq` rnf p
    rnf (QueryPagedGet t q p)  = rnf t `seq` rnf q `seq` rnf p
    rnf (QueryTimereports i u) = rnf i `seq` rnf u
    rnf (QueryCapacities i u)  = rnf i `seq` rnf u

instance ToJSON (Query a) where
    toJSON (QueryGet t q p) = object
        [ "tag"   .= ("get" :: Text)
        , "type"  .= t
        , "query" .= q
        , "path"  .= p
        ]
    toJSON (QueryPagedGet t q p) = object
        [ "tag"   .= ("paged-get" :: Text)
        , "type"  .= t
        , "query" .= q
        , "path"  .= p
        ]
    toJSON (QueryTimereports i u) = object
        [ "tag"      .= ("timereports" :: Text)
        , "interval" .= i
        , "uid"      .= u
        ]
    toJSON (QueryCapacities i u) = object
        [ "tag"      .= ("capacities" :: Text)
        , "interval" .= i
        , "uid"      .= u
        ]

-- | Encoded as JSON
instance Postgres.ToField (Query a) where
    toField = Postgres.toField . Aeson.encode

-------------------------------------------------------------------------------
-- SomeQuery instances
-------------------------------------------------------------------------------

instance Eq SomeQuery where
    SomeQuery q == SomeQuery q' = defaultEq q q';

instance Hashable SomeQuery where
    hashWithSalt salt (SomeQuery q) = hashWithSalt salt q

instance ToJSON SomeQuery where
    toJSON (SomeQuery q) = toJSON q

instance ToSchema SomeQuery where
    declareNamedSchema _ = pure $ NamedSchema (Just "PlanMill Query") mempty

instance FromJSON SomeQuery where
    parseJSON = withObject "Query" $ \obj -> do
        tag <- obj .: "tag"
        case (tag :: Text) of
            "get" -> mkSomeQueryGet
                <$> obj .: "type"
                <*> obj .: "query"
                <*> obj .: "path"
            "paged-get" -> mkSomeQueryPagedGet
                <$> obj .: "type"
                <*> obj .: "query"
                <*> obj .: "path"
            "timereports" -> mkSomeQueryTimereports
                <$> obj .: "interval"
                <*> obj .: "uid"
            "capacities" -> mkSomeQueryCapacities
                <$> obj .: "interval"
                <*> obj .: "uid"
            _ -> fail $ "Invalid tag: " ++ show tag
      where
        mkSomeQueryGet t q p = case t of
            SomeQueryTag t' -> SomeQuery (QueryGet t' q p)
        mkSomeQueryPagedGet t q p = case t of
            SomeQueryTag t' -> SomeQuery (QueryPagedGet t' q p)
        mkSomeQueryTimereports i u = SomeQuery (QueryTimereports i u)
        mkSomeQueryCapacities i u = SomeQuery (QueryCapacities i u)

-- | Encoded as JSON
instance Postgres.ToField SomeQuery where
    toField (SomeQuery q) = Postgres.toField q

instance Postgres.FromField SomeQuery where
    fromField f mbs = do
        bs <- Postgres.fromField f mbs
        case Aeson.eitherDecode bs of
            Right x  -> pure x
            Left err -> Postgres.conversionError (Aeson.AesonException err)

-------------------------------------------------------------------------------
-- Query dictionaries
-------------------------------------------------------------------------------

-- | We can recover all type parameters 'Query' can has.
queryDict
    :: (c Me, c User, c Timereports, c UserCapacities)
    => Proxy c -> (forall b. c b :- c (Vector b)) -> Query a -> Dict (c a)
queryDict p _ (QueryGet t _ _)       = queryTagDict p t
queryDict p e (QueryPagedGet t _ _)  = mapDict e (queryTagDict p t)
queryDict _ _ (QueryTimereports _ _) = Dict
queryDict _ _ (QueryCapacities _ _)  = Dict

queryTagDict
    :: (c Me, c User)
    => Proxy c -> QueryTag a -> Dict (c a)
queryTagDict _ QueryTagMe   = Dict
queryTagDict _ QueryTagUser = Dict
