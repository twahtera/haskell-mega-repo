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
import Futurice.Indexed
import Futurice.Lucid.Foundation
import Data.Char (isAlphaNum)
import Futurice.Prelude
import Prelude ()

import qualified Data.Text    as T
import qualified Generics.SOP as SOP

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- TODO: change to common field properties?
type FieldName = Text

data Field x a where
    HiddenField :: FieldName -> a -> Prism' Text a -> Field a a
    TextField :: FieldName -> Field Text Text
    -- HiddenField / StaticField

-------------------------------------------------------------------------------
-- Fields
-------------------------------------------------------------------------------

textField :: FieldName -> Lomake (Text ': xs) xs Text
textField n = liftLomake (TextField n)

hiddenField :: FieldName -> a -> Prism' Text a -> Lomake (a ': xs) xs a
hiddenField n x p = liftLomake (HiddenField n x p)

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

lomakeHtml :: forall xs m a. (Monad m, SOP.SListI xs) => NP I xs -> Lomake xs '[] a -> HtmlT m ()
lomakeHtml xs lmk =
    row_ $ large_ 12 $ do
        -- TODO: form tag
        evalStateT (lowerLomakeWithNp_ go xs lmk) mempty

        row_ $ large_ 12 $ div_ [ class_ "button-group" ] $ do
            button_ [ class_ "button success", data_ "lomake-action" "submit" ] "Submit"
            button_ [ class_ "button", data_ "lomake-action" "reset" ] "Reset"

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
    go (I value) (HiddenField name _value p) = do
        n <- inputName name
        lift $ input_
            [ data_ "lomake-id" n
            , name_ n
            , type_ "hidden"
            , value_ (review p value)
            ]

    inputName :: MonadState (Set Text) n => Text -> n Text
    inputName n0 = do
        _used <- use id
        return n1
      where
        n1 = T.map f (T.toLower n0)

        f c | isAlphaNum c = c
            | otherwise    = '-'
