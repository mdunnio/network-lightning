{-# LANGUAGE OverloadedStrings #-}

module Network.Lightning.API.NewAddress
    ( NewAddress (..)
    , NewAddressType (..)

    , newAddr
    ) where

import           Control.Monad.IO.Class    (MonadIO)
import           Data.Aeson                (FromJSON (..), ToJSON (..),
                                            Value (..), withObject, (.:))
import qualified Data.Vector               as V
import qualified Network.Haskoin.Address   as A
import           Network.Haskoin.Constants (btcTest)

import           Network.Lightning.JsonRpc (Error, JsonRpcT (..), Method (..),
                                            request)


newtype NewAddress = Address A.Address
    deriving (Show, Eq)


data NewAddressType = Bech32 | P2SH
    deriving (Show, Eq)


instance ToJSON NewAddressType where
    toJSON Bech32 = Array (V.fromList ["bech32"])
    toJSON P2SH   = Array (V.fromList ["p2sh"])


instance FromJSON NewAddress where
    parseJSON = withObject "new address" $ \o -> do
        addr <- o .: "address"
        Address <$> A.addrFromJSON btcTest addr


newAddr :: MonadIO m => NewAddressType -> JsonRpcT m (Either Error NewAddress)
newAddr = request (Method "newaddr")
