# network-lightning

Library for interfacing with c-lightning API. Currently only works over TCP using socat:

```
sudo socat TCP-LISTEN:<PORT>,reuseaddr,fork UNIX-CLIENT:/path/to/lightning/lightning-rpc
```

## example

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class             (liftIO)

import           Network.Lightning.API.Invoice      (invoice)
import           Network.Lightning.API.ListInvoices (listInvoices)
import           Network.Lightning.API.NewAddress   (NewAddressType (..),
                                                     newAddr)
import           Network.Lightning.API.NodeInfo     (getInfo)
import           Network.Lightning.Client           (runTCPClient)


main :: IO ()
main = runTCPClient host port $ do
    getInfo >>= liftIO . print
    newAddr Bech32 >>= liftIO . print
    invoice 50000 "test" "testing" Nothing >>= liftIO . print
    listInvoices Nothing >>= liftIO . print
  where
    host = "10.0.0.1"
    port = "9835"
```
