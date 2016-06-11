-- When too lazy to add type signatures for this
-- {-# LANGUAGE NoMonomorphismRestriction #-}

module Connection where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8 as BS


handleMessagesFromSock :: Socket -> (BS.ByteString -> IO a) -> IO ()
handleMessagesFromSock sock processMessages = maybeForeverIO $ do
  messages <- lift $ recv sock 512
  breakIfEmpty messages
  lift $ mapM_ processMessages (BS.lines messages)

maybeForeverIO :: MaybeT IO a -> IO ()
maybeForeverIO = void . runMaybeT . forever

breakIfEmpty :: MonadPlus f => BS.ByteString -> f ()
breakIfEmpty messages = when (messages == BS.empty) mzero
