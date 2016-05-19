{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Flowdock.TyHaxl (
    organisation,
    Haxl.initDataSource,
    Haxl.FlowdockRequest(..),
    ) where

import qualified Chat.Flowdock.REST as FD
import qualified Flowdock.Haxl      as Haxl
import           Haxl.Typed

organisation
    :: IsElem' Haxl.FlowdockRequest r (Index Haxl.FlowdockRequest r)
    => FD.ParamName FD.Organisation
    -> GenTyHaxl r u FD.Organisation
organisation = GenTyHaxl . Haxl.organisation
