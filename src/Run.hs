{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Run
  ( run
  )
where

import           Import
import qualified Data.Vector.Unboxed           as V

import           RIO.ByteString.Lazy            ( fromStrict
                                                , toStrict
                                                )
import qualified Data.Binary                   as B
import qualified Data.Binary.Get               as B
import qualified Data.Binary.Put               as B

import           Network.Socket          hiding ( recv
                                                , sendTo
                                                )
import           Network.Socket.ByteString
import           Util

data Vban = Vban
  { srProtocol :: Word8
  , nb :: Word8
  , channels :: Word8
  , dataFormat :: Word8
  , streamName :: ByteString
  , frameCounter :: Word32
  , audio :: V.Vector Int16
  } deriving (Show)

instance B.Binary Vban where
  put (Vban formatSr formatNbs formatBnc formatBit streamName nuFrame audio) = do
    B.putByteString "VBAN"
    B.putWord8 formatSr
    B.putWord8 formatNbs
    B.putWord8 formatBnc
    B.putWord8 formatBit
    B.putByteString streamName
    B.putWord32le nuFrame
    V.mapM_ B.putInt16le audio
  get = do
    B.skip 4
    formatSr   <- B.getWord8
    formatNbs  <- B.getWord8
    formatBnc  <- B.getWord8
    formatBit  <- B.getWord8
    streamName <- B.getByteString 16
    nuFrame    <- B.getWord32le
    audio      <- V.replicateM (2 * (fromIntegral formatNbs + 1)) B.getInt16le
    return $ Vban formatSr formatNbs formatBnc formatBit streamName nuFrame audio

loop :: Socket -> AddrInfo -> Double -> RIO App ()
loop rx txAddr gain = do
  b <- liftIO $ recv rx 4096

  let
    vban = B.decode $ fromStrict b
    samples :: Double
    samples = 1 + fromIntegral (nb vban)
    peak    = V.maximum $ V.map abs (audio vban)

    availableGain :: Double
    availableGain =
      abs (fromIntegral (minBound @Int16)) / (fromIntegral peak + 1)

    multGain = 1.0001 ** samples
    nextGain = min (multGain * gain) availableGain

    gAudio   = V.map (round . (* nextGain) . realToFrac) (audio vban)

  logDebug . displayShow $ (samples, gain)

  let b' = toStrict $ B.encode (vban { audio = gAudio })
  _ <- liftIO $ sendTo rx b' (addrAddress txAddr)
  loop rx txAddr nextGain

run :: RIO App ()
run = do
  logInfo "We're inside the application!"
  opts   <- appOptions `fmap` ask

  addrRx <- parseDatagramAddr $ optionsRxAddr opts
  sockRx <- liftIO
    $ socket (addrFamily addrRx) (addrSocketType addrRx) (addrProtocol addrRx)
  liftIO $ bind sockRx (addrAddress addrRx)

  addrTx <- parseDatagramAddr $ optionsTxAddr opts

  let gain = 10 ** (optionsGainPerSampleDb opts / 20)

  loop sockRx addrTx gain
