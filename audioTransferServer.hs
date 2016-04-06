import Data.Bits
import Network.Socket
import Network.BSD
import Data.List
import Control.Concurrent
import Control.Concurrent.STM
import System.IO
import Control.Monad
import Authentication
import AudioTransferTypes


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
       users <- newUsersSTM
       -- Loop forever waiting for connections.  Ctrl-C to abort.
       procRequests users sock

    where
          -- | Process incoming connection requests
          procRequests :: Users -> Socket -> IO ()
          procRequests users mastersock =
              do (connsock, clientaddr) <- accept mastersock
                 handlerfunc clientaddr "Client connnected"
                 forkIO $ procMessages users connsock clientaddr
                 procRequests users mastersock

          -- | Process incoming messages
          procMessages :: Users -> Socket -> SockAddr -> IO ()
          procMessages users connsock clientaddr =
              do connhdl <- socketToHandle connsock ReadMode
                 hSetBuffering connhdl LineBuffering
                 messages <- hGetContents connhdl
                 mapM_ (handle users clientaddr) (lines messages)
                 hClose connhdl
                 plainHandler clientaddr "Client disconnected"

          handle :: Users -> SockAddr -> String -> IO()
          handle users clientaddr message = do
            let msg = words message
            let messageType = head msg
            let usernm = msg !! 1
            let password = msg !! 2
            loggedIn <- atomically $ isLoggedIn usernm users
            when (messageType == "register") $
              register usernm password users
            when (messageType == "login") $
              logInUser usernm password users
            when (messageType == "logout") $
              logOutUser usernm users
            when (messageType == "data" && loggedIn) $
              plainHandler clientaddr message


-- A simple handler that prints incoming packets
plainHandler :: HandlerFunc
plainHandler addr msg =
    putStrLn $ "From " ++ show addr ++ ": " ++ msg
--logInUser "martinho" "soufixei" users
main = do
  users <- newUsersSTM
  openConnection "10514" plainHandler
