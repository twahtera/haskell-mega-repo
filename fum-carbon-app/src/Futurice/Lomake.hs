{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Futurice.Lomake (
    module Futurice.Lomake,
    -- * Re-exports
    module Futurice.Indexed,
    ) where

import Control.Lens               (review, use)
import Control.Monad.State.Strict
import Data.Char                  (isAlphaNum)
import Data.Maybe                 (isNothing)
import Data.Swagger               (NamedSchema (..))
import Futurice.Generics
import Futurice.Indexed
import Futurice.Lucid.Foundation
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

data Field x a where
    -- TODO: change order
    HiddenField :: FieldName -> Prism' Text a -> Field a a
    TextField   :: FieldName -> Field Text Text -- TODO: add regex validation
    EnumField   :: FieldName -> EnumFieldOpts a -> Field (Maybe a) a

data EnumFieldOpts a = EnumFieldOpts
    { efoValues    :: [a]
    , efoToApiData :: a -> Text
    , efoToHtml    :: a -> Html ()
    }

defaultEnumFieldOpts
    :: (Enum a, Bounded a, ToHttpApiData a)
    => (a -> Html ())
    -> EnumFieldOpts a
defaultEnumFieldOpts h = EnumFieldOpts
    { efoValues    = [ minBound .. maxBound ]
    , efoToApiData = toUrlPiece
    , efoToHtml    = h
    }

-------------------------------------------------------------------------------
-- Fields
-------------------------------------------------------------------------------

textField :: FieldName -> Lomake (Text ': xs) xs Text
textField n = liftLomake (TextField n)

hiddenField :: FieldName -> Prism' Text a -> Lomake (a ': xs) xs a
hiddenField n p = liftLomake (HiddenField n p)

enumField :: FieldName -> EnumFieldOpts a -> Lomake (Maybe a ': xs) xs a
enumField n opts = liftLomake (EnumField n opts)

-------------------------------------------------------------------------------
-- Lomake
-------------------------------------------------------------------------------

data Lomake :: [*] -> [*] -> * -> * where
    LomakePure  ::  a                                   -> Lomake xs xs a
    LomakeAp    ::  Field x a -> Lomake xs ys (a -> b)  -> Lomake (x ': xs) ys b

instance IxFunctor Lomake where
    f <<$>> LomakePure x  = LomakePure (f x)
    f <<$>> LomakeAp x y  = LomakeAp x ((f .) <<$>> y)

instance IxApply Lomake where
    LomakePure f <<*>> z  = f <<$>> z
    LomakeAp x y <<*>> z  = LomakeAp x (flip <<$>> y <<*>> z)

instance IxApplicative Lomake where
    ipure = LomakePure

liftLomake :: Field x a -> Lomake (x ': xs) xs a
liftLomake x = LomakeAp x (LomakePure id)

lowerLomake :: forall g xs a. Applicative g => (forall y x. Field y x -> g x) -> Lomake xs '[] a -> g a
lowerLomake nt = go where
    go :: forall ys b. Lomake ys '[] b -> g b
    go (LomakePure x)          = pure x
    go (LomakeAp field rest)   = flip id <$> nt field <*> go rest

lowerLomakeWithNp :: forall g h xs a. Applicative g => (forall y x. h y -> Field y x -> g x) -> NP h xs -> Lomake xs '[] a -> g a
lowerLomakeWithNp nt = go where
    go :: forall ys b. NP h ys -> Lomake ys '[] b -> g b
    go Nil       (LomakePure x)          = pure x
    go (h :* hs) (LomakeAp field rest)   = flip id <$> nt h field <*> go hs rest

lowerLomakeWithNp_ :: forall g h xs z a. Applicative g => (forall x y. h y -> Field y x -> g z) -> NP h xs -> Lomake xs '[] a -> g ()
lowerLomakeWithNp_ nt = go where
    go :: forall ys b. NP h ys -> Lomake ys '[] b -> g ()
    go Nil       (LomakePure _)          = pure ()
    go (h :* hs) (LomakeAp field rest)   = nt h field *> go hs rest

-------------------------------------------------------------------------------
-- Markup
-------------------------------------------------------------------------------

data FormOptions = FormOptions
    { foName :: !Text
    , foUrl  :: !Link
    }
  deriving (Show)

lomakeHtml :: forall xs m a. (Monad m, SOP.SListI xs) => FormOptions -> NP I xs -> Lomake xs '[] a -> HtmlT m ()
lomakeHtml formOpts xs lmk =
    row_ formAttributes $ large_ 12 $ do
        evalStateT (lowerLomakeWithNp_ go xs lmk) mempty

        row_ $ large_ 12 $ div_ [ class_ "button-group" ] $ do
            button_ [ class_ "button success", data_ "lomake-action" "submit", disabled_ "disabled" ] "Submit"
            button_ [ class_ "button", data_ "lomake-action" "reset", disabled_ "disabled" ] "Reset"

  where
    formAttributes =
        [ data_ "lomake-form" $ foName formOpts
        , data_ "lomake-form-submit" $ toUrlPiece $ foUrl formOpts
        ]

    go :: I y -> Field y x -> StateT (Set Text) (HtmlT m) ()
    go (I value) (TextField name) = do
        n <- inputName name
        lift $ row_ $ large_ 12 $ label_ $ do
            toHtml name
            input_
                [ data_ "lomake-id" n
                , name_ n
                , type_ "text"
                , value_ value
                ]

    go (I value) (HiddenField name p) = do
        n <- inputName name
        lift $ input_
            [ data_ "lomake-id" n
            , name_ n
            , type_ "hidden"
            , value_ (review p value)
            ]

    go (I value) (EnumField name opts) = do
        n <- inputName name
        lift $ row_ $ large_ 12 $ label_ $ do
            toHtml name
            select_ [ data_ "lomake-id" n, name_ n ] $ do
                when (isNothing value) $
                    optionSelected_ True [] "-"

                for_ (efoValues opts) $ \v ->
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
-- FromJSON
-------------------------------------------------------------------------------

lomakeParseJSON :: Lomake xs '[] a -> Value -> Aeson.Parser a
lomakeParseJSON lmk = Aeson.withObject "Lomake" $ \obj ->
    evalStateT (lowerLomake (go obj) lmk) mempty
  where
    go :: Aeson.Object -> Field y x -> StateT (Set Text) Aeson.Parser x
    go obj (TextField name) = do
        n <- inputName name
        v <- lift $ obj Aeson..: n
        pure v
    go obj (HiddenField name p) = do
        n <- inputName name
        v <- lift $ obj Aeson..: n
        maybe (fail "Hidden field in wrong format") pure (v ^? p)
    go obj (EnumField name opts) = do
        n <- inputName name
        v <- lift $ obj Aeson..: n
        case filter ((v ==) .  efoToApiData opts) (efoValues opts) of
            []    -> fail "No matching enum value"
            (x:_) -> pure x

-------------------------------------------------------------------------------
-- ToSchema
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Lomake Request
-------------------------------------------------------------------------------

class HasLomake a where
    type LomakeFields a :: [*]
    lomake :: Lomake (LomakeFields a) '[] a

-- | A newtype to allow parsing different 'Lomake'
newtype LomakeRequest a = LomakeRequest { getLomakeRequest :: a }

instance HasLomake a => FromJSON (LomakeRequest a) where
    parseJSON = fmap LomakeRequest . lomakeParseJSON lomake

-- | TODO HasLomake a =>
instance ToSchema (LomakeRequest a) where
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
