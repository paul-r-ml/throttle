{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
{-# OPTIONS -Wall #-}
module Main where

import Control.Concurrent      (forkIO,threadDelay)
import Control.Concurrent.Chan (Chan,writeChan,newChan,getChanContents)
import Control.Monad           (forever, unless)
import Data.Data               (Data,Typeable)
import Network                 (PortID(PortNumber),listenOn)
import Network.Socket hiding (listen,recv,send)
import Network.Socket.ByteString (recv,sendAll)
import System.Console.CmdArgs  (cmdArgs,(&=),help,summary,def,opt)
import System.Posix            (Handler(Ignore),installHandler,sigPIPE)
import qualified Data.ByteString as B

data Throttle = Throttle
  { listen  :: Int
  , host    :: String
  , port    :: Int
  , speed   :: Float
  , logging :: Bool
  } deriving (Show,Data,Typeable)
  
options :: Throttle
options = Throttle 
  { speed   = def &= opt (1.6::Float) &= help "Speed in KB/s, e.g. 1.6 (0 for no limit)"
  , host    = "127.0.0.1"
  , port    = 80
  , listen  = 8000
  , logging = def &= help "Log about events on the console."
  }
  &= summary "Throttle v1.0, (C) Chris Done 2010"
  &= help "Listens on port <listen> and proxies a throttled \
          \connection to <host> on <port> at speed <speed>KB/s."

main :: IO ()
main = do
  _ <- installHandler sigPIPE Ignore Nothing
  cmdArgs options >>= start

start :: Throttle -> IO ()
start Throttle{..} = withSocketsDo $ do
  c <- newTeller logging
  listener <- listenOn (PortNumber . fromIntegral $ listen)
  forever $ do
    (client,_) <- accept listener
    tell c $ [show client,": New connection on port ",show port]
    _ <- forkIO $ do
      server <- connectToServer
      tell c $ [show client,": ",show server,": Connected to server at "
               ,host,":",show port, " with delay ", show delay]
      let proxyTo f t = forkIO (proxyToWithChan c f t) >> return ()
      client `proxyTo` server
      server `proxyTo` client
    return ()
  where connectToServer = do
          addrinfos <- getAddrInfo Nothing (Just host) (Just $ show port)
          let serveraddr = head addrinfos
          server <- socket (addrFamily serveraddr) Stream defaultProtocol
          connect server (addrAddress serveraddr)
          return server

        proxyToWithChan c from to = do 
          mapData from to
          close c from to
          return ()

        mapData from to = do
          msg <- recv from bufferSize
          unless (B.null msg) $ do
            _ <- sendAll to msg
            unless (speed == 0) $ threadDelay delay
            mapData from to

        bufferSize = 1024 * 4 :: Int
        delay = round $ (fromIntegral $ (1000*1000) * bufferSize) / (speed * 1024)
                    

        close c a b = do
          tell c $ [show a,":",show b,": Closing connections."]
          sClose a
          sClose b


-- | Create a new console logger.
newTeller :: Bool -> IO (Maybe (Chan String))
newTeller False = return Nothing
newTeller True = do
  c <- newChan
  _ <- forkIO $ do
    getChanContents c >>= mapM_ putStrLn
  return $ Just c

-- | Tell the user something on the console.
tell :: Maybe (Chan String) -> [String] -> IO ()
tell (Just c) = writeChan c . concat
tell Nothing  = const $ return ()
