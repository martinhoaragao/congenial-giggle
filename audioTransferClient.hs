import Control.Concurrent
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString.Lazy
import Prelude hiding (getContents)
import qualified Data.ByteString.Lazy.Char8 as BS
import Control.Monad
import Control.Monad.Trans

import Connection

openConnection :: HostName      -- ^ Remote hostname, or 127.0.0.1
               -> String        -- ^ Port number or name; 514 is default
               -> IO Socket

openConnection hostname port =
    do -- Look up the hostname and port.  Either raises an exception
       -- or returns a nonempty list.  First element in that list
       -- is supposed to be the best option.
       addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
       let serveraddr = head addrinfos

       -- Establish a socket for communication
       sock <- socket (addrFamily serveraddr) Stream defaultProtocol

       -- Mark the socket for keep-alive handling since it may be idle
       -- for long periods of time
       setSocketOption sock KeepAlive 1

       -- Connect to server
       connect sock (addrAddress serveraddr)

       forkIO $ printMessages sock

       -- Save off the socket
       return sock

printMessages sock = handleMessagesFromSock sock BS.putStrLn 

audioTransfer sock msg = send sock (BS.pack msg)


test1 = do
  connection <- openConnection "127.0.0.1" "10514"
  audioTransfer connection "register martinho 1234"
  audioTransfer connection "login martinho 1234"
  audioTransfer connection "data martinho I can now send message!"
  audioTransfer connection "logout martinho 1234"
  audioTransfer connection "data martinho Can I send message?"
  audioTransfer connection "data martinho YES!"
  close connection

test2 = do
  connection <- openConnection "127.0.0.1" "10514"
  audioTransfer connection "register joaquim 1234"
  audioTransfer connection "data joaquim Message after register!"
  audioTransfer connection "logout joaquim 1234"
  audioTransfer connection "data joaquim Message after logout!"
  audioTransfer connection "login joaquim 1234"
  audioTransfer connection "data joaquim Message after login!"
  audioTransfer connection "logout joaquim 1234"
  audioTransfer connection "data joaquim Message after logout!"
  audioTransfer connection "login joaquim 4123"
  audioTransfer connection "data joaquim Message after bad login!"
  audioTransfer connection "logout joaquim 4123"
  audioTransfer connection "data joaquim Message after bad logout!"
  close connection

test3 = do
  connection <- openConnection "127.0.0.1" "10514"
  audioTransfer connection "register martinho 1234"
  audioTransfer connection "login martinho 1234"
  audioTransfer connection "data martinho I can now send message!"
  audioTransfer connection "logout martinho 1234"
  audioTransfer connection "data martinho Can I send message?"
  audioTransfer connection "data martinho YES!"
  close connection


main = do
  connection <- openConnection "127.0.0.1" "10514"
  forever $ do
    text <- getLine
    audioTransfer connection text
  close connection
