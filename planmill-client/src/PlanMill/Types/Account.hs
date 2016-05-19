{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
module PlanMill.Types.Account (Account(..), Accounts, AccountId) where

import PlanMill.Internal.Prelude
import Prelude                   ()

import PlanMill.Types.Identifier (HasIdentifier (..), Identifier)

type AccountId = Identifier Account
type Accounts = Vector Account

data Account = Account
    { _saId                 :: !AccountId
    , saName                :: !Text
    , saLineOfBusiness      :: !(Maybe Text)
    , saShippingCity        :: !(Maybe Text)
    , saHandlingFee         :: !(Maybe Int)
    --, saBusinessId          :: !Text -- @TODO: [String,Int,null]
    --, saVatId               :: !Text -- @TODO: [String,Int,null]
    , saInvoiceVat          :: !(Maybe Int)
    , saRevenueLastYear     :: !(Maybe Int)
    , saIndustry            :: !(Maybe Int)
    , saInvoiceEmail        :: !(Maybe Text)
    --, saShippingPostalCode  :: !Text -- @TODO: [String,Int,null]
    , saType                :: !(Maybe Int)
    , saBlog                :: !(Maybe Text)
    , saParentAccountName   :: !(Maybe Text)
    , saTwitter             :: !(Maybe Text)
    --, saOwnerName           :: !Text -- @TODO: [String,Int,null]
    , saShippingCountry     :: !(Maybe Int)
    --, saEdiCode             :: !Text -- @TODO: [String,Int,null]
    , saInvoiceChannel      :: !(Maybe Int)
    , saRfReferenceNumber   :: !(Maybe Int)
    , saBillingCountry      :: !(Maybe Int)
    , saModified            :: !(Maybe UTCTime)
    , saSupplierAccountName :: !(Maybe Text)
    , saModifiedBy          :: !(Maybe Text)
    , saActiveContacts      :: !(Maybe Int)
    --, saFax                 :: !Text -- @TODO: [String,Int,null]
    , saEmail               :: !(Maybe Text)
    , saOwner               :: !(Maybe Int)
    , saWebsite             :: !(Maybe Text)
    , saSupplierAccount     :: !(Maybe Text)
    , saCreated             :: !(Maybe UTCTime)
    , saFacebook            :: !(Maybe Text)
    , saRevenueThisYear     :: !(Maybe Int)
    , saServiceLevel        :: !(Maybe Int)
    , saPassive             :: !(Maybe Int)
    , saInvoiceNetOperator  :: !(Maybe Int)
    --, saInvoiceNetAddress   :: !(Maybe Text) -- @TODO: [String,Int,null])
    , saCombineInvoices     :: !(Maybe Int)
    --, saPhone               :: !Text -- @TODO: [String,Int,null]
    --, saCreatedBy           :: !Text -- @TODO: [String,Int,null]
    --, saBillingPostalCode   :: !Text -- @TODO: [String,Int,null]
    , saShippingAddress     :: !(Maybe Text)
    , saBillingAddress      :: !(Maybe Text)
    , saTermsOfPayment      :: !(Maybe Int)
    , saBillingCity         :: !(Maybe Text)
    }
    deriving (Eq, Ord, Show, Read, Generic, Typeable)

makeLenses ''Account
deriveGeneric ''Account

instance HasIdentifier Account Account where
    identifier = saId

instance Hashable Account
instance NFData Account
instance AnsiPretty Account
instance Binary Account
instance HasStructuralInfo Account where structuralInfo = sopStructuralInfo
instance HasSemanticVersion Account

instance FromJSON Account where
    parseJSON = withObject "Account" $ \obj ->
        Account <$> obj .: "id"
                <*> obj .: "name"
                <*> obj .:? "lineOfBusiness"
                <*> obj .: "shippingCity"
                <*> obj .:? "handlingFee"
                -- <*> obj .: "businessId"
                -- <*> obj .: "vatId"
                <*> obj .: "invoiceVat"
                <*> obj .: "revenueLastYear"
                <*> obj .: "industry"
                <*> obj .: "invoiceEmail"
                -- <*> obj .: "shippingPostalCode"
                <*> obj .: "type"
                <*> obj .: "blog"
                <*> obj .: "parentAccountName"
                <*> obj .: "twitter"
                -- <*> obj .: "ownerName"
                <*> obj .: "shippingCountry"
                -- <*> obj .: "ediCode"
                <*> obj .: "invoiceChannel"
                <*> obj .: "rfReferenceNumber"
                <*> obj .: "billingCountry"
                <*> optional (getU <$> obj .: "modified")
                <*> obj .: "supplierAccountName"
                <*> obj .: "modifiedBy"
                <*> obj .: "activeContacts"
                -- <*> obj .: "fax"
                <*> obj .: "email"
                <*> obj .: "owner"
                <*> obj .: "website"
                <*> obj .: "supplierAccount"
                <*> optional (getU <$> obj .: "created")
                <*> obj .: "facebook"
                <*> obj .: "revenueThisYear"
                <*> obj .: "serviceLevel"
                <*> obj .: "passive"
                <*> obj .: "invoiceNetOperator"
                -- <*> obj .: "invoiceNetAddress"
                <*> obj .: "combineInvoices"
                -- <*> obj .: "phone"
                -- <*> obj .: "createdBy"
                -- <*> obj .: "billingPostalCode"
                <*> obj .: "shippingAddress"
                <*> obj .: "billingAddress"
                <*> obj .: "termsOfPayment"
                <*> obj .: "billingCity"
