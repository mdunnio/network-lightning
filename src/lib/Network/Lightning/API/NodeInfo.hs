{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Network.Lightning.API.NodeInfo
    ( NodeInfo (..)
    , getInfo
    ) where

import           Control.Monad.IO.Class    (MonadIO)
import           Data.Aeson                (FromJSON (..), ToJSON (..),
                                            Value (..), withObject, (.:))
import           Data.Aeson.Casing         (snakeCase)
import           Data.Aeson.TH             (constructorTagModifier,
                                            defaultOptions, deriveJSON,
                                            fieldLabelModifier)
import           Data.Text                 (Text)
import qualified Data.Vector               as V
import           Network.Haskoin.Block     (BlockHeight)
import           Network.Haskoin.Constants (Network, btc, btcTest)
import           Network.Haskoin.Keys      (PubKeyI)

import           Network.Lightning.JsonRpc (Error, JsonRpcT (..), Method (..),
                                            request)


type Alias   = Text
type Color   = Text
type Version = Text


data NodeInfo = NodeInfo
    { nodeId      :: PubKeyI
    , alias       :: Alias
    , color       :: Color
    , address     :: [Text]
    , binding     :: [Binding]
    , version     :: Version
    , blockheight :: BlockHeight
    , network     :: Network
    } deriving (Eq, Show)


instance FromJSON NodeInfo where
    parseJSON = withObject "node info" $ \o -> do
        nodeId'      <- o .: "id"
        alias'       <- o .: "alias"
        color'       <- o .: "color"
        address'     <- o .: "address"
        binding'     <- o .: "binding"
        version'     <- o .: "version"
        blockheight' <- o .: "blockheight"
        network'     <- parseNetwork <$> o .: "network"
        return $ NodeInfo nodeId' alias' color' address' binding' version' blockheight' network'


parseNetwork :: String -> Network
parseNetwork "bitcoin" = btc
parseNetwork "testnet" = btcTest
parseNetwork _         = error "network not supported"


data Binding = Binding
    { bType    :: Text
    , bAddress :: Text
    , bPort    :: Int
    } deriving (Eq, Show)


deriveJSON defaultOptions { fieldLabelModifier = snakeCase . drop 1, constructorTagModifier = snakeCase . drop 1 } ''Binding


data NodeInfoParams = NodeInfoParams
    deriving (Eq, Show)


instance ToJSON NodeInfoParams where
    toJSON _ = Array V.empty


getInfo :: MonadIO m => JsonRpcT m (Either Error NodeInfo)
getInfo = request (Method "getinfo") NodeInfoParams
