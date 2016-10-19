{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}
module PlanMill.Types.Query (
    Query (..),
    QueryTypes,
    queryType,
    QueryTag (..),
    SomeQuery(..),
    SomeResponse(..),
    queryToRequest,
    queryDict,
    queryTagDict,
    ) where

import Prelude ()
import PlanMill.Internal.Prelude
import Data.Binary               (Binary (..), Put)
import Data.Binary.Tagged (StructuralInfo (..))
import Data.Constraint
import Data.GADT.Compare         (GEq (..), defaultEq)
import Data.Swagger              (NamedSchema (..), ToSchema (..))
import Data.Type.Equality
import Generics.SOP              (All, hcmap, hcollapse, hcpure)
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
import PlanMill.Types.User           (User, UserId, Users)
import PlanMill.Types.UserCapacity   (UserCapacities)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Arbitrary queries are tagged by this values
data QueryTag a where
    QueryTagMe   :: QueryTag Me
    QueryTagUser :: QueryTag User
    -- Remember to update HasStruturalInfo instance

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

instance Binary SomeQueryTag where
    put (SomeQueryTag QueryTagMe)   = put (0 :: Word8)
    put (SomeQueryTag QueryTagUser) = put (1 :: Word8)

    get = get >>= \n -> case (n :: Word8) of
        0 -> pure $ SomeQueryTag QueryTagMe
        1 -> pure $ SomeQueryTag QueryTagUser
        _ -> fail $ "Invalid tag " ++ show n

instance ToJSON (QueryTag a) where
    toJSON QueryTagMe   = String "me"
    toJSON QueryTagUser = String "user"

instance FromJSON SomeQueryTag where
    parseJSON (String "me")   = pure $ SomeQueryTag QueryTagMe
    parseJSON (String "user") = pure $ SomeQueryTag QueryTagUser
    parseJSON (String t)      = fail $ "Invalid tag: " ++ show t
    parseJSON v               = typeMismatch "QueryTag" v

instance HasStructuralInfo (QueryTag a) where
    structuralInfo _ = StructuralInfo "QueryTag"
        [[ NominalType "QueryTagMe"
        ,  NominalType "QueryTagUser"
        ]]

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

-- Unfortunately we cannot autoderive this
instance HasStructuralInfo (Query a) where
    structuralInfo _ = StructuralInfo "SomeQuery"
        [ [ structuralInfo (Proxy :: Proxy (QueryTag ()))
          , structuralInfo (Proxy :: Proxy QueryString)
          , structuralInfo (Proxy :: Proxy UrlParts)
          ]
        , [ structuralInfo (Proxy :: Proxy (QueryTag ()))
          , structuralInfo (Proxy :: Proxy QueryString)
          , structuralInfo (Proxy :: Proxy UrlParts)
          ]
        , [ structuralInfo (Proxy :: Proxy (Interval Day))
          , structuralInfo (Proxy :: Proxy UserId)
          ]
        , [ structuralInfo (Proxy :: Proxy (Interval Day))
          , structuralInfo (Proxy :: Proxy UserId)
          ]
        ]

-------------------------------------------------------------------------------
-- QueryTypes
-------------------------------------------------------------------------------

-- | Possible 'Query' types
type QueryTypes = '[ Timereports, UserCapacities
    , Me, Vector Me
    , User, Users
    ]

queryTagType :: QueryTag a -> NS ((:~:) a) QueryTypes
queryTagType QueryTagMe   = S (S (Z Refl))
queryTagType QueryTagUser = S (S (S (S (Z Refl))))

queryTagVectorType :: QueryTag a -> NS ((:~:) (Vector a)) QueryTypes
queryTagVectorType QueryTagMe   = S (S (S (Z Refl)))
queryTagVectorType QueryTagUser = S (S (S (S (S (Z Refl)))))

-- | Reflect the type of 'Query'.
queryType :: Query a -> NS ((:~:) a) QueryTypes
queryType (QueryGet t _ _)       = queryTagType t
queryType (QueryPagedGet t _ _)  = queryTagVectorType t
queryType (QueryTimereports _ _) = Z Refl
queryType (QueryCapacities _ _)  = S (Z Refl)

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


instance Binary SomeQuery where
    put (SomeQuery (QueryGet t q p)) = do
        put (0 :: Word8)
        put (SomeQueryTag t)
        put q
        put p
    put (SomeQuery (QueryPagedGet t q p)) = do
        put (1 :: Word8)
        put (SomeQueryTag t)
        put q
        put p
    put (SomeQuery (QueryTimereports i u)) = do
        put (2 :: Word8)
        put i
        put u
    put (SomeQuery (QueryCapacities i u)) = do
        put (3 :: Word8)
        put i
        put u

    get = get >>= \n -> case (n :: Word8) of
        0 -> mkSomeQueryGet <$> get <*> get <*> get
        1 -> mkSomeQueryPagedGet <$> get <*> get <*> get
        2 -> mkSomeQueryTimereports <$> get <*> get
        3 -> mkSomeQueryCapacities <$> get <*> get
        _ -> fail $ "Invalid tag: " ++ show n

mkSomeQueryGet :: SomeQueryTag -> QueryString -> UrlParts -> SomeQuery
mkSomeQueryGet t q p = case t of
    SomeQueryTag t' -> SomeQuery (QueryGet t' q p)

mkSomeQueryPagedGet :: SomeQueryTag -> QueryString -> UrlParts -> SomeQuery
mkSomeQueryPagedGet t q p = case t of
    SomeQueryTag t' -> SomeQuery (QueryPagedGet t' q p)

mkSomeQueryTimereports :: Interval Day -> UserId -> SomeQuery
mkSomeQueryTimereports i u = SomeQuery (QueryTimereports i u)

mkSomeQueryCapacities :: Interval Day -> UserId -> SomeQuery
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
    :: forall c a. All c QueryTypes
    => Proxy c -> Query a -> Dict (c a)
queryDict pc q = f (queryType q)
  where
    f :: NS ((:~:) a) QueryTypes -> Dict (c a)
    f xs = hcollapse (hcmap pc (\Refl -> K Dict) xs)

queryTagDict
    :: (c Me, c User)
    => Proxy c -> QueryTag a -> Dict (c a)
queryTagDict _ QueryTagMe   = Dict
queryTagDict _ QueryTagUser = Dict

-------------------------------------------------------------------------------
-- SomeResponse
-------------------------------------------------------------------------------

-- | An existential type bundling the 'Query' and it's response.
--
-- This can be serialised and deserialised, which is great!
data SomeResponse where
    MkSomeResponse :: Query a -> a -> SomeResponse

instance Binary SomeResponse where
    put (MkSomeResponse q r) = put' q r
      where
        put' :: forall a. Query a -> a -> Put
        put' q' r' = case queryDict (Proxy :: Proxy Binary) q' of
            Dict -> put (SomeQuery q') >> put r'

    get = do
        SomeQuery q <- get
        case queryDict (Proxy :: Proxy Binary) q of
            Dict -> do
                r <- get
                pure (MkSomeResponse q r)

instance NFData SomeResponse where
    rnf (MkSomeResponse q a) = case queryDict (Proxy :: Proxy NFData) q of
        Dict -> rnf q `seq` rnf a

instance HasSemanticVersion SomeResponse

instance HasStructuralInfo SomeResponse where
    structuralInfo _ =
        StructuralInfo "SomeResponse" [ queryInfo : responseInfo ]
      where
        queryInfo = structuralInfo (Proxy :: Proxy (Query ()))
        responseInfo = hcollapse infos

        infos :: NP (K StructuralInfo) QueryTypes
        infos = hcpure (Proxy :: Proxy HasStructuralInfo) f

        f :: forall a. HasStructuralInfo a => K StructuralInfo a
        f = K $ structuralInfo (Proxy :: Proxy a)

instance ToSchema SomeResponse where
    declareNamedSchema _ = pure $ NamedSchema (Just "Some planmill query response") mempty
