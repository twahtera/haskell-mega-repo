{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
#endif

module Futurice.App.FUM.Command (
    module Futurice.App.FUM.Command,
    ) where

import Futurice.Prelude
import Futurice.Generics
import Futurice.Lomake
import Futurice.Lucid.Foundation (toHtml)
import Prelude ()

import Futurice.App.FUM.Types

import qualified Personio

-------------------------------------------------------------------------------
-- CreateEmployee
-------------------------------------------------------------------------------

data CreateEmployee = CreateEmployee
    { _cePersonioId :: !Personio.EmployeeId
    , _ceLogin      :: !Login
    , _ceEmail      :: !Email
    , _ceStatus     :: !Status
    }
  deriving (Show, Typeable, Generic)

type CreateEmployeeLomake =
    Lomake (LomakeFields CreateEmployee) '[] CreateEmployee

createEmployeeLomake :: CreateEmployeeLomake
createEmployeeLomake = CreateEmployee
    <<$>> hiddenField "personioId" Personio._EmployeeId
    <<*>> textField "login"
    <<*>> textField "email"
    <<*>> enumField "status" (defaultEnumFieldOpts (toHtml . show)) -- TODO!

instance HasLomake CreateEmployee where
    type LomakeFields CreateEmployee = 
        '[ Personio.EmployeeId
         , Text
         , Text
         , Maybe Status
         ]
    lomake = createEmployeeLomake

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

-- TODO: should be derived Lomake CreateEmployeesFields '[] CreateEmployee

deriveGeneric ''CreateEmployee
instance NFData CreateEmployee
instance ToSchema CreateEmployee where
    declareNamedSchema = sopDeclareNamedSchema
instance ToJSON CreateEmployee where
    toJSON = sopToJSON
    toEncoding = sopToEncoding
instance FromJSON CreateEmployee where
    parseJSON = sopParseJSON
    
