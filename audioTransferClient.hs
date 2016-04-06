import Network.Socket
import System.IO
import Control.Monad

data AudioTransferHandle =
    AudioTransferHandle { slHandle :: Handle
                        , slProgram :: String
                        }

openConnection :: HostName             -- ^ Remote hostname, or localhost
        -> String               -- ^ Port number or name; 514 is default
        -> String               -- ^ Name to log under
        -> IO AudioTransferHandle      -- ^ Handle to use for logging
openConnection hostname port progname =
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

       -- Make a Handle out of it for convenience
       h <- socketToHandle sock WriteMode

       -- We're going to set buffering to BlockBuffering and then
       -- explicitly call hFlush after each message, below, so that
       -- messages get logged immediately
       hSetBuffering h (BlockBuffering Nothing)

       -- Save off the socket, program name, and server address in a handle
       return $ AudioTransferHandle h progname

audioTransfer :: AudioTransferHandle -> String -> IO ()
audioTransfer audioTransferh msg =
    do hPutStrLn (slHandle audioTransferh) msg
       -- Make sure that we send data immediately
       hFlush (slHandle audioTransferh)

closeConnection :: AudioTransferHandle -> IO ()
closeConnection audioTransferh = hClose (slHandle audioTransferh)

test1 = do
  connection <- openConnection "localhost" "10514" "tcptest"
  audioTransfer connection "register martinho 1234"
  audioTransfer connection "login martinho 1234"
  audioTransfer connection "data martinho I can now send message!"
  audioTransfer connection "logout martinho 1234"
  audioTransfer connection "data martinho Can I send message?"
  audioTransfer connection "data martinho YES!"
  closeConnection connection

test2 = do
  connection <- openConnection "localhost" "10514" "tcptest"
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
  closeConnection connection

test3 = do
  connection <- openConnection "localhost" "10514" "tcptest"
  audioTransfer connection "register martinho 1234"
  audioTransfer connection "login martinho 1234"
  audioTransfer connection "data martinho I can now send message!"
  audioTransfer connection "logout martinho 1234"
  audioTransfer connection "data martinho Can I send message?"
  audioTransfer connection "data martinho YES!"
  closeConnection connection


main = do
  connection <- openConnection "localhost" "10514" "tcptest"
  forever $ do
    text <- getLine
    audioTransfer connection text
  closeConnection connection
