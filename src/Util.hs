{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Silly utility module, used to demonstrate how to write a test
-- case.
module Util
  ( parseDatagramAddr
  )
where

import           RIO
import qualified RIO.Text                      as T

import           Network.Socket

parseDatagramAddr :: MonadIO m => Text -> m AddrInfo
parseDatagramAddr str = do
  ai <- liftIO $ getAddrInfo (Just info) host port
  case ai of
    a : _ -> return a
    _     -> error "can't decide addr"
 where
  info         = defaultHints { addrSocketType = Datagram }
  (host, port) = case T.break (== ':') str of
    (h, "") -> (Just . T.unpack $ h, Nothing)
    (h, p ) -> (Just . T.unpack $ h, Just . T.unpack . T.drop 1 $ p)
