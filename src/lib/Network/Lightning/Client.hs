module Network.Lightning.Client
    ( runTCPClient
    ) where

import           Control.Exception         (bracket)
import           Network.Socket            (HostName, ServiceName,
                                            SocketType (Stream), addrAddress,
                                            addrFamily, addrProtocol,
                                            addrSocketType, close, connect,
                                            defaultHints, getAddrInfo, socket)

import           Network.Lightning.JsonRpc (JsonRpcT, runJsonRpcT)


runTCPClient :: HostName -> ServiceName -> JsonRpcT IO a -> IO a
runTCPClient host port f = do
    addr <- resolve host port
    bracket (open addr) close talk
  where
    resolve host' port' = do
        let hints = defaultHints { addrSocketType = Stream }
        addr:_ <- getAddrInfo (Just hints) (Just host') (Just port')
        return addr
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock $ addrAddress addr
        return sock
    talk sock = runJsonRpcT sock f
