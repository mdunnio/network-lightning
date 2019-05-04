{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Network.Lightning.API.Invoice
    ( -- * Types
      Expiry (..)
    , Invoice
    , MilliSatoshis
    , Label
    , Description

    , invoice
    ) where

import           Control.Monad.IO.Class         (MonadIO)
import           Data.Aeson                     (FromJSON (..), ToJSON (..))
import           Data.Aeson.Casing              (snakeCase)
import           Data.Aeson.TH                  (defaultOptions, deriveJSON,
                                                 fieldLabelModifier,
                                                 omitNothingFields)
import           Data.Text                      (Text)
import           Data.Time.Clock                (DiffTime)
import           Data.Word                      (Word64)
import           Network.Haskoin.Address.Bech32 (Bech32)

import           Network.Lightning.JsonRpc      (Error, JsonRpcT (..),
                                                 Method (..), request)


type MilliSatoshis = Word64
type Label         = Text
type Description   = Text


newtype Expiry = Expiry { unExpiry :: DiffTime }
    deriving (Eq, Ord, Show, ToJSON, FromJSON)


data InvoiceParams = InvoiceParams
    { msatoshi    :: MilliSatoshis
    , label       :: Label
    , description :: Description
    , expiry      :: Maybe Expiry
    } deriving (Eq, Show)


deriveJSON defaultOptions { omitNothingFields = True } ''InvoiceParams


data Invoice = Invoice
    { paymentHash     :: Text
    , expiresAt       :: DiffTime
    , bolt11          :: Bech32
    , warningCapacity :: Maybe Text
    } deriving (Eq, Show)


deriveJSON defaultOptions { fieldLabelModifier = snakeCase } ''Invoice


-- client provided preimage is explicitly not supported
-- TODO: handle optionals
invoice :: MonadIO m
        => MilliSatoshis
        -> Label
        -> Description
        -> Maybe Expiry
        -> JsonRpcT m (Either Error Invoice)
invoice msats l desc expry = request method params
  where
    method = Method "invoice"
    params = InvoiceParams msats l desc expry
