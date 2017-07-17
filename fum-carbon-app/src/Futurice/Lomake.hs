{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
module Futurice.Lomake (
    module Futurice.Lomake,
    -- * Re-exports
    module Futurice.Indexed,
    ) where

import Control.Lens               (review, use)
import Control.Monad.State.Strict
import Data.Char                  (isAlphaNum)
import Data.Constraint
import Data.Maybe                 (isNothing)
import Data.Swagger               (NamedSchema (..))
import Futurice.Generics
import Futurice.Indexed
import Futurice.Lucid.Foundation
import Futurice.MonadTrans
import Futurice.Prelude
import Prelude ()
import Servant.API                (Link)
import Web.HttpApiData            (ToHttpApiData (..))

import qualified Data.Aeson.Compat as Aeson
import qualified Data.Text         as T
import qualified Generics.SOP      as SOP

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- TODO: change to common field properties?
type FieldName = Text

-- | A single field in the form.
--
-- * @m@ is the Monad in which we operate (parse input, get values etc)
--
-- * @x@ is a form generation input
--
-- * @a@ is a result of the field
--
data Field m x a where
    -- TODO: change order
    -- change prism to use m
    HiddenField :: FieldName -> Prism' Text a -> Field m a a
    -- TODO: make similar to ^
    TextField   :: FieldName -> Field m Text Text -- TODO: add regex validation
    EnumField   :: FieldName -> EnumFieldOpts m a -> Field m (Maybe a) a

data EnumFieldOpts m a = EnumFieldOpts
    { efoValues    :: Either (m [a]) [a]
    , efoToApiData :: a -> Text
    , efoToHtml    :: a -> Html ()
    }

defaultEnumFieldOpts
    :: (Enum a, Bounded a, ToHttpApiData a, Applicative m)
    => (a -> Html ())
    -> EnumFieldOpts m a
defaultEnumFieldOpts h = EnumFieldOpts
    { efoValues    = Right [ minBound .. maxBound ]
    , efoToApiData = toUrlPiece
    , efoToHtml    = h
    }

-------------------------------------------------------------------------------
-- Fields
-------------------------------------------------------------------------------

textField :: FieldName -> Lomake m (Text ': xs) xs Text
textField n = liftLomake (TextField n)

hiddenField :: FieldName -> Prism' Text a -> Lomake m (a ': xs) xs a
hiddenField n p = liftLomake (HiddenField n p)

enumField :: FieldName -> EnumFieldOpts m a -> Lomake m (Maybe a ': xs) xs a
enumField n opts = liftLomake (EnumField n opts)

-------------------------------------------------------------------------------
-- Lomake
-------------------------------------------------------------------------------

data Lomake :: (* -> *) -> [*] -> [*] -> * -> * where
    LomakePure  ::  a                                       -> Lomake m xs xs a
    LomakeAp    ::  Field m x a -> Lomake m xs ys (a -> b)  -> Lomake m (x ': xs) ys b

instance IxFunctor (Lomake m) where
    f <<$>> LomakePure x  = LomakePure (f x)
    f <<$>> LomakeAp x y  = LomakeAp x ((f .) <<$>> y)

instance IxApply (Lomake m) where
    LomakePure f <<*>> z  = f <<$>> z
    LomakeAp x y <<*>> z  = LomakeAp x (flip <<$>> y <<*>> z)

instance IxApplicative (Lomake m) where
    ipure = LomakePure

liftLomake :: Field m x a -> Lomake m (x ': xs) xs a
liftLomake x = LomakeAp x (LomakePure id)

lowerLomake
    :: forall m g xs a. (Applicative g, Applicative m)
    => (forall y x. Field m y x -> g (m x))
    -> Lomake m xs '[] a -> g (m a)
lowerLomake nt = getCompose . go where
    go :: forall ys b. Lomake m ys '[] b -> Compose g m b
    go (LomakePure x)          = pure x
    go (LomakeAp field rest)   = flip id <$> Compose (nt field) <*> go rest

lowerLomakeWithNp
    :: forall m g h xs a. (Applicative g, Applicative m)
    => (forall y x. h y -> Field m y x -> g (m x))
    -> NP h xs -> Lomake m xs '[] a -> g (m a)
lowerLomakeWithNp nt np = getCompose . go np where
    go :: forall ys b. NP h ys -> Lomake m ys '[] b -> Compose g m b
    go Nil       (LomakePure x)          = pure x
    go (h :* hs) (LomakeAp field rest)   = flip id <$> Compose (nt h field) <*> go hs rest

lowerLomakeWithNp_
    :: forall m g h xs z a. (Applicative g, Applicative m)
    => (forall x y. h y -> Field m y x -> g (m z))
    -> NP h xs -> Lomake m xs '[] a -> g (m ())
lowerLomakeWithNp_ nt np = getCompose . go np where
    go :: forall ys b. NP h ys -> Lomake m ys '[] b -> Compose g m ()
    go Nil       (LomakePure _)          = pure ()
    go (h :* hs) (LomakeAp field rest)   = Compose (nt h field) *> go hs rest

lowerLomakeWithNpTrans_
    :: forall m t h xs z a. (MonadTrans' t, Monad m)
    => (forall x y. h y -> Field m y x -> t m z)
    -> NP h xs -> Lomake m xs '[] a -> t m ()
lowerLomakeWithNpTrans_ nt np = go np \\ (transform' :: Monad m :- Monad (t m)) where
    go :: forall ys b. Monad (t m) => NP h ys -> Lomake m ys '[] b -> t m ()
    go Nil       (LomakePure _)          = return ()
    go (h :* hs) (LomakeAp field rest)   = nt h field >> go hs rest

-------------------------------------------------------------------------------
-- Markup
-------------------------------------------------------------------------------

data FormOptions = FormOptions
    { foName :: !Text
    , foUrl  :: !Link
    }
  deriving (Show)

lomakeHtml :: forall xs m a. (Monad m, SOP.SListI xs) => FormOptions -> NP I xs -> Lomake m xs '[] a -> HtmlT m ()
lomakeHtml formOpts xs lmk =
    row_ formAttributes $ large_ 12 $ do
        evalStateT (getComposeTrans $ lowerLomakeWithNpTrans_ go xs lmk) mempty

        row_ $ large_ 12 $ div_ [ class_ "button-group" ] $ do
            button_ [ class_ "button success", data_ "lomake-action" "submit", disabled_ "disabled" ] "Submit"
            button_ [ class_ "button", data_ "lomake-action" "reset", disabled_ "disabled" ] "Reset"

  where
    formAttributes =
        [ data_ "lomake-form" $ foName formOpts
        , data_ "lomake-form-submit" $ toUrlPiece $ foUrl formOpts
        ]

    go :: I y -> Field m y x -> ComposeTrans (StateT (Set Text)) HtmlT m ()
    go (I value) (TextField name) = ComposeTrans $ do
        n <- inputName name
        lift $ row_ $ large_ 12 $ label_ $ do
            toHtml name
            input_
                [ data_ "lomake-id" n
                , name_ n
                , type_ "text"
                , value_ value
                ]

    go (I value) (HiddenField name p) = ComposeTrans $ do
        n <- inputName name
        lift $ input_
            [ data_ "lomake-id" n
            , name_ n
            , type_ "hidden"
            , value_ (review p value)
            ]

    go (I value) (EnumField name opts) = ComposeTrans $ do
        values <- either (lift' . lift') return $ efoValues opts
        n <- inputName name
        lift $ row_ $ large_ 12 $ label_ $ do
            toHtml name
            select_ [ data_ "lomake-id" n, name_ n ] $ do
                when (isNothing value) $
                    optionSelected_ True [] "-"

                for_ values $ \v ->
                    optionSelected_ (fmap p value == Just (p v)) [ value_ $ p v ] $ h v
      where
        p = efoToApiData opts
        h x = hoist (return . runIdentity) (efoToHtml opts x)

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

inputName :: MonadState (Set Text) n => Text -> n Text
inputName n0 = do
    _used <- use id
    return n1
  where
    -- n1 = T.map f (T.toLower n0)
    n1 = T.map f n0

    f c | isAlphaNum c = c
        | otherwise    = '-'

-------------------------------------------------------------------------------
-- LomakeErrors
-------------------------------------------------------------------------------

-- TODO use LomakeError
class Monad m => MonadLomakeError m where
    lomakeError :: String -> m a

-------------------------------------------------------------------------------
-- FromJSON
-------------------------------------------------------------------------------

lomakeParseJSON :: forall m xs a. MonadLomakeError m => Lomake m xs '[] a -> Value -> Aeson.Parser (m a)
lomakeParseJSON lmk = Aeson.withObject "Lomake" $ \obj ->
    evalStateT (lowerLomake (go obj) lmk) mempty
  where
    go :: Aeson.Object -> Field m y x -> StateT (Set Text) Aeson.Parser (m x)
    go obj (TextField name) = do
        n <- inputName name
        v <- lift $ obj Aeson..: n
        pure (pure v)
    go obj (HiddenField name p) = do
        n <- inputName name
        v <- lift $ obj Aeson..: n
        maybe (fail "Hidden field in a wrong format") (pure . pure) (v ^? p)
    go obj (EnumField name opts) = do
        n <- inputName name
        v <- lift $ obj Aeson..: n
        lift $ case efoValues opts of
            Right values -> parse fail pure values (efoToApiData opts) v
            Left m       -> pure $ m >>= \values ->
                            parse lomakeError id values (efoToApiData opts) v
      where
        parse :: Applicative n => (String -> n b) -> (c -> b) -> [c] -> (c -> Text) -> Text -> n b
        parse f s values str v = case filter ((v ==) . str) values of
            []    -> f "No matching enum value"
            (x:_) -> pure (s x)
        {-# INLINE parse #-}

-------------------------------------------------------------------------------
-- ToSchema
-------------------------------------------------------------------------------

-- TODO

-------------------------------------------------------------------------------
-- Lomake Request
-------------------------------------------------------------------------------

class HasLomake m a | a -> m where
    type LomakeFields a :: [*]
    lomake :: Lomake m (LomakeFields a) '[] a

-- | A newtype to allow parsing different 'Lomake'
newtype LomakeRequest m a = LomakeRequest { getLomakeRequest :: m a }

instance (HasLomake m a, MonadLomakeError m) => FromJSON (LomakeRequest m a) where
    parseJSON = fmap LomakeRequest . lomakeParseJSON lomake

-- | TODO HasLomake a, Something m =>
instance ToSchema (LomakeRequest m a) where
    declareNamedSchema _ = pure $ NamedSchema (Just "Lomake") mempty

-------------------------------------------------------------------------------
-- Lomake Response
-------------------------------------------------------------------------------

data LomakeResponse
    = LomakeResponseNoop            -- ^ Do nothing
    | LomakeResponseRedirect !Text  -- ^ redirect to the url
  deriving (Eq, Ord, Show, Typeable, Generic)

instance ToJSON LomakeResponse
instance FromJSON LomakeResponse
instance ToSchema LomakeResponse
