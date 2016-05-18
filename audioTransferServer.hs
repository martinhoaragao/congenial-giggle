import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString.Lazy
import Prelude hiding (getContents)
import qualified Data.ByteString.Lazy.Char8 as BS
import Control.Concurrent
import Control.Monad

import Authentication
import Connection
--import AudioTransferTypes


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

       -- Create user data structure
       users <- newUsers
       -- Loop forever waiting for connections.  Ctrl-C to abort.
       procRequests users sock

       close sock

    where
          -- | Process incoming connection requests
          procRequests :: Users -> Socket -> IO ()
          procRequests users mastersock =
              do (connsock, clientAddr) <- accept mastersock
                 handlerfunc clientAddr "Client connnected"
                 forkIO $ procMessages users connsock clientAddr
                 procRequests users mastersock

          -- | Process incoming messages
          procMessages :: Users -> Socket -> SockAddr -> IO ()
          procMessages users connSock clientAddr = do
            userConnected <- newUserConnected clientAddr
            handleMessagesFromSock connSock (handle users userConnected connSock)
            plainHandler clientAddr "Client disconnected"

          handle :: Users -> UserConnected -> Socket -> BS.ByteString -> IO()
          handle users userConnected connSock message = do
            let msg = words . BS.unpack $ message
            let [messageType, usernm, password] = take 3 msg
            putStrLn $ messageType ++" " ++ usernm ++ " "++  password
            loggedIn <- isLoggedIn userConnected users
            case messageType of
              "register"  -> register usernm password users
              "login"     -> logInUser usernm password userConnected users
              "delete"    -> when loggedIn $ deleteUser userConnected users
              "logout"    -> when loggedIn $ logOutUser userConnected users
              "data"      -> when loggedIn $ deliver connSock message


deliver connSock msg = do
  let message = BS.pack . unwords . drop 1 . words . BS.unpack $ msg
  send connSock message

  return ()

-- A simple handler that prints incoming packets to server
plainHandler :: HandlerFunc
plainHandler addr msg =
    putStrLn $ "From " ++ show addr ++ ": " ++ msg

main = openConnection "10514" plainHandler
