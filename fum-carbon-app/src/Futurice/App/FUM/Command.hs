{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
#endif

module Futurice.App.FUM.Command (
    module Futurice.App.FUM.Command,
    ) where

import Futurice.Generics
import Futurice.Lomake
import Futurice.Lucid.Foundation (toHtml)
import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.Types

import qualified Personio

-------------------------------------------------------------------------------
-- Common Command Stuff
-------------------------------------------------------------------------------

newtype CommandM a = CommandM { unCommandM :: ReaderT World (Either String) a }
  deriving (Functor, Applicative, Monad, MonadError String)

runCommandM :: World -> CommandM a -> Either String a
runCommandM world (CommandM m) = runReaderT m world

instance MonadLomakeError CommandM where
    lomakeError = throwError

-------------------------------------------------------------------------------
-- CommandAction
-------------------------------------------------------------------------------

-- | Apply command.
class CommandAction c where
    applyCommand :: c -> CommandM World

-------------------------------------------------------------------------------
-- CreateEmployee
-------------------------------------------------------------------------------

data CreateEmployee = CreateEmployee
    { _ceId         :: !(Identifier Employee)
    , _cePersonioId :: !Personio.EmployeeId
    , _ceLogin      :: !Login
    , _ceStatus     :: !Status
    }
  deriving (Show, Typeable, Generic)

instance CommandAction CreateEmployee where
    applyCommand _c = throwError "create-employee not implemented"

type CreateEmployeeLomake =
    Lomake CommandM (LomakeFields CreateEmployee) '[] CreateEmployee

createEmployeeLomake :: CreateEmployeeLomake
createEmployeeLomake = CreateEmployee
    <<$>> hiddenField "id" _IdentifierText
    <<*>> hiddenField "personioId" Personio._EmployeeId
    <<*>> textField "login" -- TODO: change to hidden
    <<*>> enumField "status" (defaultEnumFieldOpts (toHtml . show)) -- TODO, better HTML

instance HasLomake CommandM CreateEmployee where
    type LomakeFields CreateEmployee =
        '[ (Identifier Employee)
         , Personio.EmployeeId
         , Text
         , Maybe Status
         ]
    lomake = createEmployeeLomake

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

deriveGeneric ''CreateEmployee
instance NFData CreateEmployee
instance ToSchema CreateEmployee where
    declareNamedSchema = sopDeclareNamedSchema
instance ToJSON CreateEmployee where
    toJSON = sopToJSON
    toEncoding = sopToEncoding
instance FromJSON CreateEmployee where
    parseJSON = sopParseJSON

