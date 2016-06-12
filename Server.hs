module Server where

import           Control.Concurrent
import           Control.Monad
import           Data.ByteString.Lazy           (fromStrict, toStrict)
import qualified Data.ByteString.Lazy.Char8     as BS
import           Network.Socket                 hiding (recv, recvFrom, send,
                                                 sendTo)
import           Network.Socket.ByteString      as KEK
import           Network.Socket.ByteString.Lazy
import           Prelude                        hiding (getContents)

import           Authentication
import           Connection
import           ConsultsResponses
import           Data.Char                      (isDigit)

import           Data.Maybe

type HandlerFunc = SockAddr -> String -> IO ()

openConnection port superAddress superPort handlerfunc = withSocketsDo $
    do -- Create data structures
       users <- newUsers
       consultsResponses <- newConsultsResponses

       -- Look up the hostname and port.  Either raises an exception
       -- or returns a nonempty list.  First element in that list
       -- is supposed to be the best option.
       addrinfos <- getAddrInfo Nothing (Just superAddress) (Just superPort)
       let serveraddr = head addrinfos

       -- Establish a socket for communication
       sockSuperServer <- socket (addrFamily serveraddr) Stream defaultProtocol

       -- Mark the socket for keep-alive handling since it may be idle
       -- for long periods of time
       setSocketOption sockSuperServer KeepAlive 1

       -- Connect to server
       connect sockSuperServer (addrAddress serveraddr)

       forkIO $
         forever $ do
           text <- getLine
           deliver text sockSuperServer

       forkIO $ handleMessagesFromSock sockSuperServer (procSuperMessages users consultsResponses sockSuperServer)

       -----------------------------------------------------------------------

       -- Look up the port.  Either raises an exception or returns
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

       -- Loop forever waiting for connections.  Ctrl-C to abort.
       procRequests users consultsResponses sock sockSuperServer

       close sock

    where
          -- | Receive messages from SuperServer
          procSuperMessages :: Users -> ConsultsResponses -> Socket -> BS.ByteString -> IO ()
          procSuperMessages users consultsResponses sockSuperServer message = do
            let msg = words . BS.unpack $ message
            let [messageType, file_name, usernm] = take 3 msg
            let responses = recoverAddrPair . drop 3 $ msg
            serverName <- getSocketName sockSuperServer
            let username = drop (length(show serverName)) $ usernm
            putStrLn $ messageType ++" " ++ usernm ++ " " ++ username
            case messageType of
              "consult" -> consultToSuper users consultsResponses file_name usernm sockSuperServer
              "response" -> mapM_ (addConsultResponse consultsResponses username) responses



          -- | Process incoming connection requests
          procRequests :: Users -> ConsultsResponses -> Socket -> Socket -> IO ()
          procRequests users consultsResponses mastersock sockSuperServer =
              do (connsock, clientAddr) <- accept mastersock
                 handlerfunc clientAddr "Client connected"
                 forkIO $ procMessages users consultsResponses connsock clientAddr sockSuperServer
                 procRequests users consultsResponses mastersock sockSuperServer

          -- | Process incoming messages
          procMessages :: Users -> ConsultsResponses -> Socket -> SockAddr -> Socket -> IO ()
          procMessages users consultsResponses connSock clientAddr sockSuperServer = do
            userConnected <- newUserConnected connSock
            handleMessagesFromSock connSock (handle users consultsResponses userConnected connSock sockSuperServer)
            plainHandler clientAddr "Client disconnected"
            loggedIn <- isLoggedIn userConnected users
            when loggedIn $ logOutUser userConnected users

          handle :: Users -> ConsultsResponses -> UserConnected -> Socket -> Socket -> BS.ByteString -> IO()
          handle users consultsResponses userConnected connSock sockSuperServer message = do
            let msg = words . BS.unpack $ message
            let [messageType, usernm@file_name] = take 2 msg
            putStrLn $ messageType ++" " ++ usernm -- ++ " "++  password   -- Test if Server Receiving
            sockAddr <- getPeerName connSock
            (host, _) <- getNameInfo [NI_NOFQDN, NI_NUMERICHOST] True False sockAddr
            loggedIn <- isLoggedIn userConnected users
            case messageType of
              "register"  -> register usernm (msg !! 2) users
              "login"     -> logInUser usernm (msg !! 2) userConnected users
              "delete"    -> when loggedIn $ deleteUser userConnected users
              "logout"    -> when loggedIn $ logOutUser userConnected users
              "download"  -> when loggedIn $ sendFile users consultsResponses userConnected file_name connSock sockSuperServer
              "response"  -> addConsultResponse consultsResponses usernm (fromJust host, msg !! 2)
              _ -> return ()

consultClients users file_name username = do
  connectedUsersAddr <- getConnectedUsersAddr users
  let consult = BS.pack . unwords $ ["consult", file_name, username]
  mapM_ (`Network.Socket.ByteString.Lazy.send` consult) connectedUsersAddr

consultToSuper users consultsResponses file_name username sockSuperServer = do
  addConsult username consultsResponses
  consultClients users file_name username
  threadDelay $ 1 * 1000 * 1000
  consultResponsesAddr <- getConsultResponses username consultsResponses
  let reply = ["response", username] ++ concatMap (\(addr, port) -> [addr, port]) consultResponsesAddr
  deliver (unwords reply) sockSuperServer

sendFile users consultsResponses userConnected file_name connSock sockSuperServer = do
  username <- getConnectedUsername userConnected
  addConsult username consultsResponses
  consultClients users file_name username
  threadDelay $ 1 * 1000 * 1000
  consultResponsesAddr <- getConsultResponses username consultsResponses
  let found = null consultResponsesAddr
  when found $ do
    serverName <- getSocketName sockSuperServer
    let reply = ["download", file_name, show serverName ++ username]
    deliver (unwords reply) sockSuperServer
    threadDelay $ 5 * 1000 * 1000
  consultResponses <- getConsultResponses username consultsResponses
  let ifFound = if null consultResponses then "notfound" else "found"
  let reply = ["response", file_name, ifFound] ++ concatMap (\(addr, port) -> [addr, port]) consultResponses
  deliver (unwords reply) connSock
  print reply
  return ()

recoverAddrPair [] = []
recoverAddrPair (addr:port:l) = (addr,port) : recoverAddrPair l

deliver msg connSock = do
  KEK.sendAll connSock (toStrict $ BS.pack  msg)
  return ()

-- A simple handler that prints incoming packets to server
plainHandler :: HandlerFunc
plainHandler addr msg =
    putStrLn $ "From " ++ show addr ++ ": " ++ msg

main = do
  putStrLn "Porta TCP do Server"
  port <- getLine
  putStrLn "Indique o endereço TCP do SuperServer"
  superAddress <- getLine

  openConnection port superAddress "10499" plainHandler
