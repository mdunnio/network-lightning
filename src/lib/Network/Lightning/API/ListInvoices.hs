{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Network.Lightning.API.ListInvoices
    ( Invoices
    , listInvoices
    ) where

import           Control.Monad.IO.Class        (MonadIO)
import           Data.Aeson                    (FromJSON (..), ToJSON,
                                                withObject, (.:))
import           Data.Aeson.TH                 (defaultOptions, deriveJSON)
import           Data.Maybe                    (fromMaybe)

import           Network.Lightning.API.Invoice (Invoice, Label)
import           Network.Lightning.JsonRpc     (Error, JsonRpcT, Method (..),
                                                request)


newtype ListInvoicesParams = ListInvoicesParams { labels :: [Label] }
    deriving (Eq, Show)


deriveJSON defaultOptions ''ListInvoicesParams


newtype Invoices = Invoices [Invoice]
    deriving (Eq, Show, ToJSON)


instance FromJSON Invoices where
    parseJSON = withObject "invoices" $ \o -> Invoices <$> o .: "invoices"


listInvoices :: MonadIO m => Maybe [Label] -> JsonRpcT m (Either Error Invoices)
listInvoices lbs = request method params
  where
    method = Method "listinvoices"
    params = fromMaybe [] lbs
