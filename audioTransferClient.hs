import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8     as BS
import           Data.Maybe                     (fromJust)
import           Network.Socket                 hiding (recv, recvFrom, send,
                                                 sendTo)
import           Network.Socket.ByteString.Lazy
import           Prelude                        hiding (getContents)
import           System.Directory


import           AudioTransferTypes
import           Connection
import           UDP



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

       forkIO $ handleMessagesFromSock sock (processOneMessage sock)

       -- Save off the socket
       return sock

processOneMessage sock message = do
    let msg = words . BS.unpack $ message
    let [messageType, file_name, username] = take 3 msg
    putStrLn $ messageType ++ " " ++ file_name --consult musica.mp3
    case messageType of
      "consult" -> processConsultRequest sock file_name username
      "response" -> processConsultResponse sock msg


processConsultRequest :: Socket -> String -> String -> IO ()
processConsultRequest connection file_name username = do
    k <- getCurrentDirectory
    l <- getDirectoryContents k
    let found = elem file_name l
    when found $ do
      let res = unwords ["response", username, file_name]
      void $ audioTransfer connection res


processConsultResponse :: Socket -> [String] -> IO()
processConsultResponse sockSend msg = do
    let ([messageType, file_name, wasFound, numberHosts], hosts) = splitAt 4 msg
    let userUDPConnections = words hosts
    if not $ read wasFound then putStrLn "Ficheiro não encontrado no servidor!" >> return ()
    else do
      target_connection <- send_probe_requests userUDPConnections --UDP
      case target_connection of
        Nothing -> putStrLn "Não há utilizadores com ligação estável!" >> return ()
        Just (ip, port) -> send_file_request file_name ip port


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

test4 = do
  connection <- openConnection "127.0.0.1" "10514"
  audioTransfer connection "register martinho 1234"
  audioTransfer connection "login martinho 1234"
  audioTransfer connection "data martinho I can now send message!"
  audioTransfer connection "download martinho test.mp3"
  audioTransfer connection "logout martinho 1234"

  close connection

main = do
  connection <- openConnection "127.0.0.1" "10514"
  forever $ do
    text <- getLine
    audioTransfer connection text
  close connection
