module Server where

import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString.Lazy.Char8     as BS
import           Network.Socket                 hiding (recv, recvFrom, send,
                                                 sendTo)
import           Network.Socket.ByteString.Lazy
import           Network.Socket.ByteString as KEK
import           Prelude                        hiding (getContents)
import Data.ByteString.Lazy (fromStrict, toStrict)

import           Authentication
import           Connection
import           ConsultsResponses
import           Data.Char                      (isDigit)
--import           Data.IP

import           Data.Maybe

type HandlerFunc = SockAddr -> String -> IO ()

openConnection :: String              -- ^ Port number or name; 514 is default
               -> HandlerFunc         -- ^ Function to handle incoming messages
               -> IO ()
openConnection port handlerfunc = withSocketsDo $
    do -- Look up the port.  Either raises an exception or returns
       -- a nonempty list.
       addrinfos <- getAddrInfo
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                    Nothing (Just port)
       let serveraddr = head addrinfos

       -- Create a socket
       sock <- socket (addrFamily serveraddr) Stream defaultProtocol

       -- Bind it to the address we're listening to
       bindSocket sock (addrAddress serveraddr)

       -- Start listening for connection requests.  Maximum queue size
       -- of 5 connection requests waiting to be accepted.
       listen sock 5

       -- Create data structures
       users <- newUsers
       consultsResponses <- newConsultsResponses
       -- Loop forever waiting for connections.  Ctrl-C to abort.
       procRequests users consultsResponses sock

       close sock

    where
          -- | Process incoming connection requests
          procRequests :: Users -> ConsultsResponses -> Socket -> IO ()
          procRequests users consultsResponses mastersock =
              do (connsock, clientAddr) <- accept mastersock
                 handlerfunc clientAddr "Client connected"
                 forkIO $ procMessages users consultsResponses connsock clientAddr
                 procRequests users consultsResponses mastersock

          -- | Process incoming messages
          procMessages :: Users -> ConsultsResponses -> Socket -> SockAddr -> IO ()
          procMessages users consultsResponses connSock clientAddr = do
            userConnected <- newUserConnected connSock
            handleMessagesFromSock connSock (handle users consultsResponses userConnected connSock)
            plainHandler clientAddr "Client disconnected"

          handle :: Users -> ConsultsResponses -> UserConnected -> Socket -> BS.ByteString -> IO()
          handle users consultsResponses userConnected connSock message = do
            let msg = words . BS.unpack $ message
            let [messageType, usernm@file_name] = take 2 msg
            putStrLn $ messageType ++" " ++ usernm -- ++ " "++  password   -- Test if Server Receiving
            sockAddr <- getPeerName connSock
            (host, service) <- getNameInfo [NI_NOFQDN, NI_NUMERICHOST] True True sockAddr
            loggedIn <- isLoggedIn userConnected users
            case messageType of
              "register"  -> register usernm (msg !! 2) users
              "login"     -> logInUser usernm (msg !! 2) userConnected users
              "delete"    -> when loggedIn $ deleteUser userConnected users
              "logout"    -> when loggedIn $ logOutUser userConnected users
              "download"  -> when loggedIn $ sendFile users consultsResponses userConnected file_name connSock
              "response"  -> addConsultResponse usernm (fromJust host, msg !! 2) consultsResponses
              _ -> return ()

sendFile users consultsResponses userConnected file_name connSock = do
  username <- getConnectedUsername userConnected
  addConsult username consultsResponses
  connectedUsersAddr <- getConnectedUsersAddr users
  let consult = BS.pack . unwords $ ["consult", file_name, username]
  mapM_ (`Network.Socket.ByteString.Lazy.send` consult) connectedUsersAddr
  threadDelay $ 1 * 1000 * 1000
  consultResponsesAddr <- getConsultResponses username consultsResponses
  let ifFound = if null consultResponsesAddr then "notfound" else "found"
  let reply = ["response", file_name, ifFound] ++ concatMap (\(addr, port) -> [addr, port]) consultResponsesAddr
  deliver (unwords reply) connSock
  print reply

  return ()


  --juntar resultados,
  --enviar "response file_name wasFound nHosts host1 port1 host2 port2 host3 port3" a connSock

deliver msg connSock = do
  KEK.sendAll connSock (toStrict $ BS.pack  msg)

  return ()

-- A simple handler that prints incoming packets to server
plainHandler :: HandlerFunc
plainHandler addr msg =
    putStrLn $ "From " ++ show addr ++ ": " ++ msg

main = openConnection "10514" plainHandler
