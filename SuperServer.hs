module SuperServer where

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
                 handlerfunc clientAddr "Server connected"
                 forkIO $ procMessages users consultsResponses connsock clientAddr
                 procRequests users consultsResponses mastersock

          -- | Process incoming messages
          procMessages :: Users -> ConsultsResponses -> Socket -> SockAddr -> IO ()
          procMessages users consultsResponses connSock clientAddr = do
            userConnected <- newUserConnected connSock
            handleMessagesFromSock connSock (handle users consultsResponses userConnected connSock)
            plainHandler clientAddr "Server disconnected"

          handle :: Users -> ConsultsResponses -> UserConnected -> Socket -> BS.ByteString -> IO()
          handle users consultsResponses userConnected connSock message = do
            let msg = words . BS.unpack $ message
            let [messageType, usernm@file_name] = take 2 msg
            let responses = recoverAddrPair . drop 2 $ msg
            putStrLn $ messageType ++" " ++ usernm -- ++ " "++  password   -- Test if Server Receiving
            loggedIn <- isLoggedIn userConnected users
            case messageType of
              "register"  -> register usernm (msg !! 2) users
              "login"     -> logInUser usernm (msg !! 2) userConnected users
              "delete"    -> when loggedIn $ deleteUser userConnected users
              "logout"    -> when loggedIn $ logOutUser userConnected users
              "download"  -> when loggedIn $ sendFile users consultsResponses file_name (msg !! 2) connSock
              "response"  -> mapM_ (addConsultResponse consultsResponses usernm) responses
              _ -> return ()

sendFile users consultsResponses file_name username connSock = do
  addConsult username consultsResponses
  connectedUsersAddr <- getConnectedUsersAddr users
  let consult = BS.pack . unwords $ ["consult", file_name, username]
  mapM_ (`Network.Socket.ByteString.Lazy.send` consult) connectedUsersAddr
  threadDelay $ 2 * 1000 * 1000
  consultResponsesAddr <- getConsultResponses username consultsResponses
  let reply = ["response", file_name, username] ++ concatMap (\(addr, port) -> [addr, port]) consultResponsesAddr
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

main = openConnection "10499" plainHandler
