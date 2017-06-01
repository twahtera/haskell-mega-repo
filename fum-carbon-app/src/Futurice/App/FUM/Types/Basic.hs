{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
-- | Basic types
module Futurice.App.FUM.Types.Basic where

import Futurice.IdMap   (HasKey (..))
import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.Types.Identifier
import Futurice.App.FUM.Types.Status

import qualified Personio as P

-- TODO:
type SshKey = Text
type Login = Text
-- | Email ending with @futurice.com
type Email = Text
-- | Any email
type RawEmail = Text
-- | We always have /some/ picture of the employee.
type Picture = Text

data GroupType
    = GroupTypeNormal
    | GroupTypeProject
    | GroupTypeServer
  deriving (Eq, Ord, Show, Typeable, Generic, Enum, Bounded)

instance NFData GroupType

-- | Employee: person
--
-- Name etc. comes from Personio.
data Employee = Employee
    { _employeeId             :: !(Identifier Employee)  -- ^ internal unique identifier
    , _employeePersonioId     :: !P.EmployeeId           -- ^ @123@, provides information to names, contract data etc.
    , _employeeStatus         :: !Status                 -- ^ "futurice status", importantly not directly the google status.
    , _employeeEmailAliases   :: ![Email]
    , _employeeSshKeys        :: ![SshKey]
    , _employeePicture        :: !Picture
    , _employeePasswordExp    :: !UTCTime                -- ^ password expiration date, does LDAP expires?
--    , _employeePassword :: FORMAT?       -- ^ will make LDAP server easy, SHA-512
    }
  deriving (Eq, Ord, Show, Typeable, Generic)

-- | What can a customer do in Futurice's internal systems.
data CustomerRight
    = CustomerRightWiki   -- ^ access to wikis
    | CustomerRightSSH    -- ^ ssh login to servers
    | CustomerRightEmail  -- ^ can be added to email lists
  deriving (Eq, Ord, Show, Typeable, Generic, Enum, Bounded)

instance NFData CustomerRight

-- | 'Customer's aren't 'Employee's.
data Customer = Customer
    { _customerId             :: !(Identifier Customer)
    , _customerLogin          :: !Login
    , _customerName           :: !Text
    , _customerSshKeys        :: ![SshKey]
    , _customerEmail          :: !RawEmail
    , _customerStatus         :: !Status
    , _customerRight          :: !(Set CustomerRight)
    , _employeeActivationDate :: !UTCTime  -- ^ like "hire-date"
    , _employeeSuspendDate    :: !UTCTime  -- ^ there's always to disable
    }
  deriving (Eq, Ord, Show, Typeable, Generic)

-- | Shared mailbox.
data Mailbox = Mailbox
    { _mailboxId           :: !(Identifier Mailbox)
    , _mailboxEmail        :: !Email
    , _mailboxEmailAliases :: ![Email]
    }
  deriving (Eq, Ord, Show, Typeable, Generic)

-- | Group: email list or access group.
data Group = Group
    { _groupId           :: !(Identifier Group)
    , _groupType         :: !GroupType
    , _groupDescription  :: !Text
    , _groupEmail        :: !(Maybe Email)
    , _groupEmailAliaes  :: ![Email]
    -- Graph wiring:
    , _groupEditor       :: !(Set (Identifier Group))  -- ^ if 'null', editors are members of the group
    , _groupEmployees    :: !(Set (Identifier Employee))
    , _groupCustomers    :: !(Set (Identifier Customer))
    }
  deriving (Eq, Ord, Show, Typeable, Generic)

-------------------------------------------------------------------------------
-- Lenses
-------------------------------------------------------------------------------

makeLenses ''Customer
makeLenses ''Employee
makeLenses ''Group
makeLenses ''Mailbox

-------------------------------------------------------------------------------
-- HasIdentifier instances
-------------------------------------------------------------------------------

instance HasKey Customer where
    type Key Customer = Identifier Customer
    key = customerId

instance HasKey Employee where
    type Key Employee = Identifier Employee
    key = employeeId

instance HasKey Group where
    type Key Group = Identifier Group
    key = groupId

instance HasKey Mailbox where
    type Key Mailbox = Identifier Mailbox
    key = mailboxId

instance HasIdentifier Customer Customer where identifier = key
instance HasIdentifier Employee Employee where identifier = key
instance HasIdentifier Group    Group    where identifier = key
instance HasIdentifier Mailbox  Mailbox  where identifier = key

instance Entity Customer  where entityName _ = "FUM.Customer"
instance Entity Employee  where entityName _ = "FUM.Employee"
instance Entity Group     where entityName _ = "FUM.Group"
instance Entity Mailbox   where entityName _ = "FUM.Mailbox"

-------------------------------------------------------------------------------
-- instances
-------------------------------------------------------------------------------

deriveGeneric ''Customer
deriveGeneric ''Employee
deriveGeneric ''Group
deriveGeneric ''Mailbox

instance NFData Customer
instance NFData Employee
instance NFData Group
instance NFData Mailbox
