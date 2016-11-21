{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.GitHub (
    -- * Tags
    ReqTag,
    tagDict,
    SomeTag (..),
    mkTag,
    eqTag,
    -- * Types
    GHTypes,
    -- * Request / Response
    SomeRequest (..),
    SomeResponse (..),
    -- * Re-exports
    module GitHub,
    ) where

import Prelude ()
import Futurice.Prelude   hiding (Pair)
import Control.Lens       (( # ))
import Data.Aeson.Compat
       (AesonException (..), FromJSON (..), Object, ToJSON (..), eitherDecode,
       encode, object, withObject, (.:), (.=))
import Data.Aeson.Types   (Pair, Parser)
import Data.Binary        (Binary (..))
import Data.Binary.Tagged
       (HasSemanticVersion, HasStructuralInfo (..), StructuralInfo (..))
import Data.Constraint    (Dict (..))
import Data.Maybe         (mapMaybe)
import Data.Swagger       (NamedSchema (..), ToSchema (..))
import Data.Type.Equality
import Futurice.Aeson     (withValueDump)
import Futurice.Has       (In, inj)
import Futurice.List      (Append, TMap, splitAppend, tmapToNSComp)
import Generics.SOP
       ((:.:) (..), All, SList (..), SListI (..), hcollapse, hcpure)

import qualified Data.Text.Encoding                   as TE
import qualified Data.Text.Encoding.Error             as TE
import qualified Database.PostgreSQL.Simple.FromField as Postgres
import qualified Database.PostgreSQL.Simple.ToField   as Postgres

import GitHub

-- | Types of requests that can be serialised.
type GHTypes = Append Scalars (TMap Vector Collections)
type Scalars =
    '[ Organization
    , Owner
    , Repo
    , Team
    , User
    ]

type Collections =
   '[ Event
    , Issue
    , Repo
    , SimpleOrganization
    , SimpleTeam
    , SimpleUser
    ]

-- | An alias to ':~:'
type Is = (:~:)

-------------------------------------------------------------------------------
-- Tag
-------------------------------------------------------------------------------

-- | Tag of serialisable requests.
type ReqTag a = NS (Is a) GHTypes

-- | Existential tag.
data SomeTag where
    MkSomeTag :: ReqTag a -> SomeTag

instance Show SomeTag where
    showsPrec d (MkSomeTag t) = showParen (d > 10)
        $ showString "MkSomeTag (intToSomeRep "
        . showsPrec 11 (nsToInt t)
        . showString ")"

instance ToJSON SomeTag where
    toJSON (MkSomeTag t) = toJSON (nsToInt t)

instance FromJSON SomeTag where
    parseJSON v = do
        n <- parseJSON v
        maybe (fail $ "Invalid tag number: " ++ show n) (pure . mk) (intToSomeRep n)
      where
        mk :: SomeRep GHTypes -> SomeTag
        mk (MkSomeRep ns) = MkSomeTag ns

instance Binary SomeTag where
    put (MkSomeTag t) = put (nsToInt t)
    get = do
        n <- get
        maybe (fail $ "Invalid tag number: " ++ show n) (pure . mk) (intToSomeRep n)
      where
        mk :: SomeRep GHTypes -> SomeTag
        mk (MkSomeRep ns) = MkSomeTag ns

-- | Create tag for a type in 'GHTypes'.
mkTag :: In a GHTypes => ReqTag a
mkTag = inj # Refl

-- | Like 'geq'.
eqTag :: ReqTag a -> ReqTag b -> Maybe (a :~: b)
eqTag = eqRep

-------------------------------------------------------------------------------
-- SomeRep is auxiliary type
-------------------------------------------------------------------------------

data SomeRep xs where
    MkSomeRep :: NS (Is a) xs -> SomeRep xs

succSomeRep :: SomeRep xs -> SomeRep (x ': xs)
succSomeRep (MkSomeRep ns) = MkSomeRep (S ns)

intToSomeRep :: forall xs. SListI xs => Int -> Maybe (SomeRep xs)
intToSomeRep n = case sList :: SList xs of
    SNil  -> Nothing
    SCons -> case n of
        0 -> Just (MkSomeRep (Z Refl))
        _ -> succSomeRep <$> intToSomeRep (n - 1)

nsToInt :: NS f xs -> Int
nsToInt (Z _) = 0
nsToInt (S n) = 1 + nsToInt n

eqRep :: NS (Is a) xs -> NS (Is b) xs -> Maybe (a :~: b)
eqRep (Z Refl) (Z Refl) = Just Refl
eqRep (S n) (S m)       = eqRep n m
eqRep _ _               = Nothing

-------------------------------------------------------------------------------
-- Request
-------------------------------------------------------------------------------

tagDict :: All c GHTypes => Proxy c -> ReqTag a -> Dict (c a)
tagDict = repDict

-- | Existential request.
data SomeRequest where
    MkSomeRequest :: ReqTag a -> Request 'RA a -> SomeRequest

instance Eq SomeRequest where
    MkSomeRequest t r == MkSomeRequest t' r' = fromMaybe False $ do
        Refl <- eqRep t t'
        case tagDict (Proxy :: Proxy Eq) t of
            Dict -> pure (r == r')

instance Hashable SomeRequest where
    hashWithSalt salt (MkSomeRequest t r) = salt
        `hashWithSalt` (nsToInt t)
        `hashWithSalt` r

instance Show SomeRequest where
    showsPrec d (MkSomeRequest _ r) = showParen (d > 10)
        $ showString "MkSomeRequest mkTag "
        . showsPrec 11 r

instance ToJSON SomeRequest where
    toJSON (MkSomeRequest t r) = object $
        [ "tag" .= MkSomeTag t
        ] ++ requestToJSON r

instance Postgres.ToField SomeRequest where
    toField = Postgres.toField . encode

instance Postgres.FromField SomeRequest where
    fromField f mbs = do
        bs <- Postgres.fromField f mbs
        case eitherDecode bs of
            Right x  -> pure x
            Left err -> Postgres.conversionError (AesonException err)

instance ToSchema SomeRequest where
    declareNamedSchema _ = pure $ NamedSchema (Just "Some github request") mempty

requestToJSON :: Request 'RA a -> [Pair]
requestToJSON (SimpleQuery q) = simpleRequestToJSON q
requestToJSON _               = []

simpleRequestToJSON :: SimpleRequest 'RA a -> [Pair]
simpleRequestToJSON (Query ps qs) =
    [ "type"  .= ("query" :: Text)
    , "paths" .= ps
    , "query" .= queryStringToText qs
    ]
simpleRequestToJSON (PagedQuery ps qs fc) =
    [ "type"  .= ("pagedquery" :: Text)
    , "paths" .= ps
    , "query" .= queryStringToText qs
    , "count" .= fetchCountToJSON fc
    ]

instance FromJSON SomeRequest where
    parseJSON = withValueDump $ withObject "SomeRequest" $ \obj -> do
        MkSomeTag tag <- obj .: "tag"
        typ <- obj .: "type" :: Parser Text
        case repDict (Proxy :: Proxy FromJSON) tag of
            Dict -> case typ of
                "query" -> do
                    req <- queryParseJSON obj
                    pure $ MkSomeRequest tag (SimpleQuery req)
                "pagedquery" -> case ghtypeIsVector tag of
                    Nothing -> fail $ "Tag is not of vector type for paged req"
                    Just MkIsVector -> do
                        req <- pagedQueryParseJSON obj
                        pure $ MkSomeRequest tag (SimpleQuery req)
                _ -> fail $ "Unknown query type: " ++ show typ

queryParseJSON :: Object -> Parser (SimpleRequest 'RA a)
queryParseJSON obj = mkQuery
    <$> obj .: "paths"
    <*> obj .: "query"
  where
    mkQuery :: [Text] -> [(Text, Text)] -> SimpleRequest 'RA a
    mkQuery ps qs = Query ps (textToQueryString qs)

pagedQueryParseJSON :: Object -> Parser (SimpleRequest 'RA (Vector a))
pagedQueryParseJSON obj = mkPagedQuery
    <$> obj .: "paths"
    <*> obj .: "query"
    <*> obj .: "count"
  where
    mkPagedQuery :: [Text] -> [(Text, Text)] -> Maybe Word -> SimpleRequest 'RA (Vector a)
    mkPagedQuery ps qs fc = PagedQuery ps (textToQueryString qs) (fetchCountParseJSON fc)

-------------------------------------------------------------------------------
-- SomeResponse
-------------------------------------------------------------------------------

-- | Existential response.
data SomeResponse where
    MkSomeResponse :: ReqTag a -> a -> SomeResponse

instance NFData SomeResponse where
    rnf (MkSomeResponse t x) = do
        case repDict (Proxy :: Proxy NFData) t of
            Dict -> rnf x

instance Binary SomeResponse where
    put (MkSomeResponse t x) = do
        put (MkSomeTag t)
        case repDict (Proxy :: Proxy Binary) t of
            Dict -> put x

    get = do
        MkSomeTag t <- get
        case repDict (Proxy :: Proxy Binary) t of
            Dict -> do
                x <- get
                pure $ MkSomeResponse t x

instance HasSemanticVersion SomeResponse

instance HasStructuralInfo SomeResponse where
    structuralInfo _ = StructuralInfo "GitHub.SomeResponse" [responseInfo]
      where
        responseInfo = hcollapse infos

        infos :: NP (K StructuralInfo) GHTypes
        infos = hcpure (Proxy :: Proxy HasStructuralInfo) f

        f :: forall a. HasStructuralInfo a => K StructuralInfo a
        f = K $ structuralInfo (Proxy :: Proxy a)

instance ToSchema SomeResponse where
    declareNamedSchema _ = pure $ NamedSchema (Just "Some github response") mempty

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

decodeUtf8Lenient :: ByteString -> Text
decodeUtf8Lenient = TE.decodeUtf8With TE.lenientDecode

queryStringToText :: QueryString -> [(Text, Text)]
queryStringToText = mapMaybe f
  where
    f (k, v) = (,) (decodeUtf8Lenient k) . decodeUtf8Lenient <$> v

textToQueryString :: [(Text, Text)] -> QueryString
textToQueryString = fmap (bimap TE.encodeUtf8 (Just . TE.encodeUtf8))

fetchCountToJSON :: FetchCount -> Maybe Word
fetchCountToJSON (FetchAtLeast n) = Just n
fetchCountToJSON FetchAll         = Nothing

fetchCountParseJSON :: Maybe Word -> FetchCount
fetchCountParseJSON Nothing  = FetchAll
fetchCountParseJSON (Just n) = FetchAtLeast n

repDict :: All c xs => Proxy c -> NS (Is a) xs -> Dict (c a)
repDict _ (Z Refl) = Dict
repDict p (S n)    = repDict p n

data IsVector a where
    MkIsVector :: IsVector (Vector b)

ghtypeIsVector :: NS (Is a) GHTypes -> Maybe (IsVector a)
ghtypeIsVector = repIsVector'
    (Proxy :: Proxy Scalars)
    (Proxy :: Proxy Collections)

repIsVector'
    :: forall xs ys a. (SListI xs, SListI ys)
    => Proxy xs -> Proxy ys
    -> NS (Is a) (Append xs (TMap Vector ys))
    -> Maybe (IsVector a)
repIsVector' _pxs _pys ns =
    case splitAppend ns :: Either (NS (Is a) xs) (NS (Is a) (TMap Vector ys)) of
        Left _    -> Nothing
        Right ns' -> Just $ repIsVector (tmapToNSComp ns' :: NS (Is a :.: Vector) ys)

repIsVector :: NS (Is a :.: Vector) xs -> IsVector a
repIsVector (Z (Comp Refl)) = MkIsVector
repIsVector (S ns)          = repIsVector ns
