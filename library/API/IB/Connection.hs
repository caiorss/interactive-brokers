{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}
module API.IB.Connection (
    connect
    , disconnect
    , IBConnection(..)
    , Config(..)
    , IBException(..)
    , send
    , recv
    , sendRecv
    ) where

import           API.IB.Constant
import           API.IB.Data                as IB
import           API.IB.Request
import           Control.Exception
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString.Char8      as BC
import           Data.Default
import           Data.IORef
import           Data.Maybe
import           Data.Time
import           Data.Time.Zones
import           Data.Typeable
import qualified Network                    as N
import qualified Network.Simple.TCP         as S

data Config = Config
  { autoStart          :: Bool --TODO
  , debug              :: Bool --TODO
  , clientId           :: Int
  , connRetryDelaySecs :: Int
  , host               :: N.HostName
  , port               :: String
  , useSecure          :: Bool
  }

instance Default Config where
  def = Config
    { autoStart = True
    , debug = False
    , clientId = 0
    , connRetryDelaySecs = 10
    , host = "127.0.0.1"
    , port = "4001"
    , useSecure = False
    }

data IBConnection = IBConnection
  { config             :: Config
  , socket             :: S.Socket
  , accounts           :: [String]
  , serverVersion      :: Int
  , serverTime         :: LocalTime
  , serverTimeZoneDesc :: String
  , serverTimeZone     :: Maybe TZ
  , nextOrderId        :: IORef Int
  }

data IBException = ConnectionClosed | ParserFailed deriving (Show, Typeable)

instance Exception IBException

-- | Establish the connection. Throws exceptions if connection closes or
-- parsing fails. Need to close the socket myself with `S.closeSock`.
connect :: Config -> IO IBConnection
connect cfg = do
    (s, _) <- S.connectSock (host cfg) (port cfg)

    let send' msg = do
                S.send s msg
                m_bs <- S.recv s 4096
                maybe (throwIO ConnectionClosed) return m_bs

    bs <- send' $ createClientVersionMsg clientVersion

    let (Connection sv st stzd stz) = fromMaybe (throw ParserFailed) $
            A.maybeResult $ A.parse parseConnection bs

    bs' <- send' $ createClientIdMsg $ clientId cfg

    let (NextValidId i) = fromMaybe (throw ParserFailed) $
            A.maybeResult $ A.parse parseNextValidId bs'

    bs'' <- send' $ createMsg sv RequestManagedAccounts

    let (ManagedAccounts acs) = fromMaybe (throw ParserFailed) $
            A.maybeResult $ A.parse parseManagedAccounts bs''

    oid <- newIORef i

    return $ IBConnection cfg s acs sv st stzd stz oid

-- | Disconnect the connection. Closes the underlying socket.
disconnect :: IBConnection -> IO ()
disconnect con = S.closeSock (socket con)

-- | Send a request.
send :: IBConnection -> IBRequest -> IO ()
send con r@PlaceOrder{..} = S.send (socket con) (createMsg (serverVersion con) r) >> atomicModifyIORef (nextOrderId con) (\oid -> (oid + 1, ()))
send con r@CancelOrder{..} = S.send (socket con) (createMsg (serverVersion con) r) >> atomicModifyIORef (nextOrderId con) (\oid -> (oid + 1, ()))
send con r = S.send (socket con) $ createMsg (serverVersion con) r

-- | Receive up to 4096 bytes.
recv :: IBConnection -> IO BC.ByteString
recv con = do
    m_bs <- S.recv (socket con) 4096
    bs <- maybe (throw ConnectionClosed) return $ m_bs
    {-print $ BC.length bs-}
    return bs

-- | Receive a response. Mainly for debugging. This throws an error if parsing fails."
sendRecv :: IBConnection -> IBRequest -> IO IBResponse
sendRecv con r = do
    send con r
    bs <- recv con
    response <- A.parseWith (recv con) parseIBResponse bs
    maybe (throwIO ParserFailed) return $ A.maybeResult response
