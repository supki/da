{-# LANGUAGE ImportQualifiedPost #-}
module Sign
  ( Signature(..)
  , verify
  ) where

import Crypto.Hash.Algorithms (SHA512(SHA512))
import Crypto.PubKey.RSA.PKCS15 qualified as Rsa
import Data.ByteString (ByteString)

import TH (randomOrgPubKey)


newtype Signature = Signature ByteString
    deriving (Show, Eq)

verify :: Signature -> ByteString -> Bool
verify (Signature signature) xs =
  Rsa.verify (Just SHA512) randomOrgPubKey xs signature
