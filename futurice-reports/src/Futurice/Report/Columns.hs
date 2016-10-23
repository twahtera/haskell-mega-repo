{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Futurice.Report.Columns (
    -- * Report
    Report (..),
    ReportGenerated (..),
    reportParams,
    reportData,
    -- * Cell
    ReportValue (..),
    -- * Class
    ToColumns (..),
    DefaultColumns,
    ) where

import Prelude ()
import Futurice.Prelude
import Data.Swagger              (NamedSchema (..))
import Futurice.Generics
import Futurice.List
import Futurice.Time (NDT (..), IsTimeUnit (..), AsScientific)
import Futurice.Lucid.Foundation
import Generics.SOP              ((:.:) (..), All, SListI (..), hmap, hpure)
import GHC.TypeLits              (KnownSymbol, Symbol, symbolVal)
import Servant.API               (MimeRender (..))
import Servant.CSV.Cassava       (CSV', EncodeOpts (..))

import qualified Data.Csv          as Csv
import qualified Data.Tuple.Strict as S
import qualified Generics.SOP      as SOP

-- instances
import qualified FUM
import qualified GitHub as GH

-------------------------------------------------------------------------------
-- Report
-------------------------------------------------------------------------------

-- | The report.
--
-- Report is parameterised over
--
-- * @name@ - the name
-- * @params@ - the report parameteters, e.g. 'ReportGenerated'
-- * @fs@ and @a@ - report actual data
data Report (name :: Symbol) params a = Report
    { _reportParams :: !params
    , _reportData   :: !a
    }
    deriving (Typeable, Functor, Foldable, Traversable)

makeLenses ''Report
deriveGeneric ''Report

instance (NFData params, NFData a) => NFData (Report name params a) where
    rnf (Report params a) = rnf params `seq` rnf a

-------------------------------------------------------------------------------
-- Report + aeson
-------------------------------------------------------------------------------

instance (ToJSON a, ToJSON params)
    => ToJSON (Report name params a)
  where
    toJSON = sopToJSON
    toEncoding = sopToEncoding

instance (FromJSON a, FromJSON params, KnownSymbol name)
    => FromJSON (Report name params a)
  where
    parseJSON = sopParseJSON

instance (ToSchema a, ToSchema params, KnownSymbol name)
    => ToSchema (Report name params a)
  where
    declareNamedSchema p = do
        NamedSchema _ schema <- sopDeclareNamedSchema p
        pure $ NamedSchema (Just $ title ^. packed) schema
      where
        title = "Report: " <> symbolVal (Proxy :: Proxy name)

-------------------------------------------------------------------------------
-- Report + cassava + servant
-------------------------------------------------------------------------------

instance (ToColumns a, All Csv.ToField (Columns a), EncodeOpts opt)
    => MimeRender (CSV', opt) (Report name params a)
  where
    mimeRender _
        = Csv.encodeWith (encodeOpts (Proxy :: Proxy opt))
        . toColumns . _reportData

-------------------------------------------------------------------------------
-- Report + lucid
-------------------------------------------------------------------------------

instance
    ( ToColumns a
    , SOP.All ReportValue (Columns a)
    , KnownSymbol name, ToHtml params
    ) => ToHtml (Report name params a)
  where
    toHtmlRaw = toHtml

    toHtml :: forall m. Monad m => Report name params a -> HtmlT m ()
    toHtml (Report params d) = toHtml $ page_ (fromString title) pageParams $ do
        row_ $ large_ 12 $ h1_ $ fromString title
        row_ $ large_ 12 $ div_ [class_ "callout"] $ toHtml params
        div_ [class_ "futu-report-wrapper" ] $ do
            row_ $ large_ 12 $ table_ [class_ "futu-report hover"] $ do
                thead_ $ tr_ $ 
                    th_ "Some header"
                tbody_ $ for_ (toColumns d) $ \r -> tr_ {- todo: class -} $ 
                    sequenceA $ SOP.hcollapse $
                        SOP.hcmap (Proxy :: Proxy ReportValue) renderCell r
                    
      where
        title = symbolVal (Proxy :: Proxy name)
        pageParams = defPageParams
            & pageJs .~
                [ menrvaJS
                -- , $(embedJS "reports.js")
                ]

        renderCell :: forall v. ReportValue v => I v -> K (Html ()) v
        renderCell (I v) = K $ td_ $ reportValueHtml v

-------------------------------------------------------------------------------
-- Class
-------------------------------------------------------------------------------

-- | Class of types which are convertible to tabular data
--
-- Default definitions works for record like types using "Generics.SOP" machinery.
--
-- Another alternative for /scalar/ types is to wrap them:
--
-- @
-- instance ToColumns FUM.UserName where
--     type Columns FUM.UserName = '[FUM.UserName]
--         toColumns u = [I u :* Nil]
-- @
--
-- However, most of these instance should be defined in this module.
class ToColumns a where
    type Columns a :: [*]
    type Columns a =  DefaultColumns a

    toColumns :: a -> [NP I (Columns a)]
    default toColumns
        :: ( code ~ Columns a
           , '[code] ~ SOP.Code a
           , SOP.Generic a
           )
        => a -> [NP I (Columns a)]
    toColumns x = [SOP.unZ $ SOP.unSOP $ SOP.from x]

type DefaultColumns a = UnSingleton (SOP.Code a)

-------------------------------------------------------------------------------
-- Scalars
-------------------------------------------------------------------------------

instance ToColumns () where
    type Columns () = '[]

    toColumns _ = []

instance ToColumns FUM.UserName where
    type Columns FUM.UserName = '[FUM.UserName]
    toColumns u = [I u :* Nil]

-------------------------------------------------------------------------------
-- Containers
-------------------------------------------------------------------------------

instance (ToColumns a, SListI (Columns a)) => ToColumns (Maybe a) where
    type Columns (Maybe a) = TMap Maybe (Columns a)

    toColumns :: Maybe a -> [NP I (TMap Maybe (Columns a))]
    toColumns Nothing =
        [npCompToTMap (hpure inothing :: NP (I :.: Maybe) (Columns a)) ]
      where
        inothing :: forall b. (I :.: Maybe) b
        inothing = Comp (I Nothing)
    toColumns (Just a) = f <$> toColumns a
      where
        f :: NP I (Columns a) -> NP I (TMap Maybe (Columns a))
        f = npCompToTMap . hmap (Comp . fmap Just)

instance ToColumns a => ToColumns [a] where
    type Columns [a] = Columns a

    toColumns = concatMap toColumns

instance (ToColumns a, ToColumns b) => ToColumns (a, b) where
    type Columns (a, b) = Append (Columns a) (Columns b)

    toColumns (a, b) = append <$> toColumns a <*> toColumns b

instance (ToColumns a, ToColumns b) => ToColumns (S.Pair a b) where
    type Columns (S.Pair a b) = Append (Columns a) (Columns b)

    toColumns (a S.:!: b) = append <$> toColumns a <*> toColumns b

instance
    ( ToColumns a, ToColumns b
    , SListI (Columns a)
    , SListI (Columns b)
    ) => ToColumns (These a b)
  where
    type Columns (These a b) = Columns (Maybe a, Maybe b)

    toColumns (These a b) = toColumns (Just a, Just b)
    toColumns (This a)    = toColumns (Just a, Nothing :: Maybe b)
    toColumns (That b)    = toColumns (Nothing :: Maybe a, Just b)

instance ToColumns a => ToColumns (Vector a) where
    type Columns (Vector a) = Columns a

    toColumns = foldMap toColumns

instance (ToColumns k, ToColumns v) => ToColumns (Map k v) where
    type Columns (Map k v) = Append (Columns k) (Columns v)

    toColumns = ifoldMap f
      where
        f k v = append <$> toColumns k <*> toColumns v

instance (ToColumns k, ToColumns v) => ToColumns (HashMap k v) where
    type Columns (HashMap k v) = Append (Columns k) (Columns v)

    toColumns = ifoldMap f
      where
        f k v = append <$> toColumns k <*> toColumns v

-------------------------------------------------------------------------------
-- ReportGenerated
-------------------------------------------------------------------------------

-- | Report generation time.
newtype ReportGenerated = ReportGenerated { getReportGenerated :: UTCTime }
    deriving (Typeable, Generic)

instance NFData ReportGenerated
instance ToJSON ReportGenerated
instance FromJSON ReportGenerated
instance ToSchema ReportGenerated

instance ToHtml ReportGenerated where
    toHtml r = do
        "Generated at "
        toHtml . show . getReportGenerated $ r
    toHtmlRaw = toHtml

-------------------------------------------------------------------------------
-- Cell value, i.e. primitive types shown in the report
-------------------------------------------------------------------------------

-- | /TODO/ add "js type" field
class ToJSON a => ReportValue a where
    reportValueHtml :: a -> Html ()

instance ReportValue Day where
    reportValueHtml = fromString . show

instance ReportValue UTCTime where
    reportValueHtml = fromString . show

instance ReportValue FUM.UserName where
    reportValueHtml = toHtml . FUM._getUserName

instance ReportValue Text where
    reportValueHtml = toHtml

instance ReportValue a => ReportValue (Maybe a) where
    reportValueHtml = maybe (pure ()) reportValueHtml

instance ReportValue Bool where
    reportValueHtml = bool "yes" "no"

instance
    (Show a, IsTimeUnit tu, AsScientific a)
    => ReportValue (NDT tu a)
  where
    reportValueHtml (NDT x) =
        toHtmlRaw $ show x <> nbsp <> sfx
      where
        sfx  = symbolVal (Proxy :: Proxy (TimeUnitSfx tu))
        nbsp = "&nbsp;" :: String

instance ReportValue (GH.Name a) where
    reportValueHtml = reportValueHtml . GH.untagName
