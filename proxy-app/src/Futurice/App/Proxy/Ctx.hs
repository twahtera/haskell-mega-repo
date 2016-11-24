module Futurice.App.Proxy.Ctx where

import Prelude ()
import Futurice.Prelude
import Data.Pool                  (Pool)
import Database.PostgreSQL.Simple (Connection)
import Network.HTTP.Client        (Manager)
import Servant.Client             (BaseUrl)
import Servant.Proxy              (HasHttpManager (..))

-- | Context type, holds http manager and baseurl configurations
data Ctx = Ctx
    { ctxManager              :: !Manager
    , ctxPostgresPool         :: !(Pool Connection)
    , ctxReportsAppBaseurl    :: !BaseUrl
    , ctxPlanmillProxyBaseurl :: !BaseUrl
    }

instance HasHttpManager Ctx where
    httpManager = lens ctxManager $ \ctx x -> ctx { ctxManager = x }
