{-# LANGUAGE TemplateHaskell   #-}
-- | Certificate is in the separate module. Less TH overall.
module Papertrail.Certificate (papertrailStore) where

import Prelude ()
import Prelude.Compat
import Data.FileEmbed (embedFile, makeRelativeToProject)

import qualified Data.X509.CertificateStore as X509
import qualified Data.X509.Memory           as X509

-- | Papertrail SSL certificate.
papertrailStore :: X509.CertificateStore
papertrailStore = 
    X509.makeCertificateStore $ X509.readSignedObjectFromMemory $
        $(makeRelativeToProject "papertrail-bundle.pem" >>= embedFile)
