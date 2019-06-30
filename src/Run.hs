{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run
  ( run
  )
where

import           Import
import           Control.Monad                  ( replicateM )
import           Data.Word
import qualified Data.List                     as L

import           RIO.ByteString.Lazy            ( fromStrict )
import qualified RIO.ByteString                as BS
import qualified Data.Binary.Get               as B

import           Network.Socket          hiding ( recv )
import           Network.Socket.ByteString

data Vban = Vban
  { srProtocol :: Word8
  , bitmode :: Word8
  , channels :: Word8
  , dataFormat :: Word8
  , streamName :: ByteString
  , frameCounter :: Word32
  , audio :: [Int16]
  } deriving (Show)

getVban :: B.Get Vban
getVban = do
  B.skip 4
  formatSr   <- B.getWord8
  formatNbs  <- B.getWord8
  formatBnc  <- B.getWord8
  formatBit  <- B.getWord8
  streamName <- B.getByteString 16
  nuFrame    <- B.getWord32le
  audio      <- replicateM ((fromIntegral formatNbs) + 1) B.getInt16le
  return $ Vban formatSr formatNbs formatBnc formatBit streamName nuFrame audio

loop :: Socket -> RIO App ()
loop sock = do
  b <- liftIO $ recv sock 4096

  let vban = B.runGet getVban $ fromStrict b
      peak = L.maximum $ map abs (audio vban)
      bar :: Int
      bar = (fromIntegral peak) * 100 `div` 32768
  logInfo . displayShow $ replicate bar '>'
  loop sock

run :: RIO App ()
run = do
  logInfo "We're inside the application!"

  addrInfo <- liftIO $ getAddrInfo Nothing (Just "0.0.0.0") (Just "6900")
  let Just addr = L.find
        (\AddrInfo { addrSocketType } -> addrSocketType == Datagram)
        addrInfo
  sock <- liftIO
    $ socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  liftIO $ bind sock (addrAddress addr)
  loop sock
