{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Futurice.Indexed
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Prelude ()
import Web.HttpApiData            (ToHttpApiData (..))

import qualified Data.Text    as T
import qualified Generics.SOP as SOP

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

lowerLomakeWithNp :: forall g h xs a. Applicative g => (forall y x. h y -> Field y x -> g x) -> NP h xs -> Lomake xs '[] a -> g a
lowerLomakeWithNp nt = go where
    go :: forall ys b. NP h ys -> Lomake ys '[] b -> g b
    go Nil       (LomakePure x)          = pure x
    go (h :* hs) (LomakeAp field rest)   = flip id <$> nt h field <*> go hs rest -- go hs rest

lowerLomakeWithNp_ :: forall g h xs z a. Applicative g => (forall x y. h y -> Field y x -> g z) -> NP h xs -> Lomake xs '[] a -> g ()
lowerLomakeWithNp_ nt = go where
    go :: forall ys b. NP h ys -> Lomake ys '[] b -> g ()
    go Nil       (LomakePure _)          = pure ()
    go (h :* hs) (LomakeAp field rest)   = nt h field *> go hs rest -- go hs rest

-------------------------------------------------------------------------------
-- Markup
-------------------------------------------------------------------------------

lomakeHtml :: forall xs m a. (Monad m, SOP.SListI xs) => Text -> NP I xs -> Lomake xs '[] a -> HtmlT m ()
lomakeHtml formName xs lmk =
    row_ [ data_ "lomake-form" formName ] $ large_ 12 $ do
        evalStateT (lowerLomakeWithNp_ go xs lmk) mempty

        row_ $ large_ 12 $ div_ [ class_ "button-group" ] $ do
            button_ [ class_ "button success", data_ "lomake-action" "submit", disabled_ "disabled" ] "Submit"
            button_ [ class_ "button", data_ "lomake-action" "reset", disabled_ "disabled" ] "Reset"

  where
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

    inputName :: MonadState (Set Text) n => Text -> n Text
    inputName n0 = do
        _used <- use id
        return n1
      where
        n1 = T.map f (T.toLower n0)

        f c | isAlphaNum c = c
            | otherwise    = '-'
