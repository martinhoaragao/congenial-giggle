import Data.Bits
import Network.Socket
import Network.BSD
import Data.List
import System.IO
import AudioTransferTypes

data AudioTransferHandle = AudioTransferHandle { slSocket  :: Socket
                                               , slProgram :: String,
                                               , slAddress :: SockAddr
                                               }

openConnection :: HostName          -- ^ Remote hostname, or localhost
        -> String                   -- ^ Port number or name; 514 is default
        -> String                   -- ^ Name to log under
        -> IO AudioTransferHandle   -- ^ Handle to use for logging
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
       return $ SyslogHandle h progname

audioTransfer :: AudioTransferHandle -> MessageType -> String -> IO ()
audioTransfer audioTransferH messageType msg =
    sendstr sendmsg
    where code = fromEnum messageType
          sendmsg = "<" ++ show code ++ ">" ++ slProgram audioTransferH ++
                    ": " ++ msg

          -- Send until everything is done
          sendstr :: String -> IO ()
          sendstr [] = return ()
          sendstr omsg = do sent <- sendTo (slSocket audioTransferH) omsg
                                    (slAddress audioTransferH)
                            sendstr (genericDrop sent omsg)

closeConnection :: AudioTransferHandle -> IO ()
closeConnection audioTransferH = sClose (slSocket audioTransferH)
