module AudioTransferProbe where
import Network.Socket (socketToHandle)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Network.Socket.ByteString (recvFrom, sendTo)
import Network.Socket (Socket, SockAddr (SockAddrInet, SockAddrInet6))
import Network.Socket.ByteString.Lazy
import qualified Data.ByteString as BS
import Control.Concurrent.STM
import Control.Concurrent (forkIO, killThread, threadDelay)
import Data.Binary
import System.IO 
import Data.List (sortBy)
import Data.Time.Clock.POSIX
import Control.Monad (forever)

import AudioTransferHeader
import AudioTransferTypes
import AudioTransferHeader
import UDP

--Asks target for a file named file_name
send_file_request :: String -> String -> String -> IO ()
send_file_request file_name ip port = do
    sock <- getSockUDPClient ip port
    let h = Header {getTipo ='6', getSeqNum = -1, getAckNum = -1, getDataSize = (length file_name)}
    let datagram = addHeader h (toStrict $ encode file_name)
    send sock $ fromStrict datagram
    return ()

--Answers a Probe Request with a timestamp
send_probe_response :: Socket -> SockAddr -> IO ()
send_probe_response  sock sa = do
    --sock <- getSockUDPClient ip port
    time <- getPOSIXTime
    let s = init $ show time
    let dados = (toStrict $ encode s)
    let h = Header {getTipo ='5', getSeqNum = -1, getAckNum = -1, getDataSize = (BS.length dados)}
    let datagram = addHeader h dados --sends time, encoded
    sendTo sock datagram sa
    --send sock $ fromStrict datagram
    return ()


 --Must run at all times on client. Will respond to Probe Requests and File Requests
udp_handler :: IO ()
udp_handler = do
    sockServ <- getSockUDPServer
    forever $ do
        --(dados, sa) <- recvFrom sockServ 25
        (dados, sa) <- recvFrom sockServ 25
        putStrLn $ ("Recebido dados! Vindos de " ++ (show sa))
        let (SockAddrInet port host) = sa
        --let (port, host) = fromSockAddr sa
        let h@(Header t s a d) = bs2header dados
        case getMessageType h of
            PROBE_REQUEST -> send_probe_response sockServ sa
            REQUEST -> undefined

--fromSockAddr (SockAddrInet port host) = (port, host)
--fromSockAddr (SockAddrInet6 port _ host _ ) = (port, host)
--fromSockAddr (SockAddrUnix s) = error s



getProbeRequest :: BS.ByteString
getProbeRequest = addHeader (Header { getTipo = '4', getSeqNum = -1, getAckNum = -1, getDataSize = -1 }) BS.empty

send_probe_requests :: [UserConnection] -> IO (Maybe UserConnection)
send_probe_requests l = do
    responses <- newTVarIO []
    threads <- mapM (\x -> forkIO $ send_probe x responses) l
    threadDelay $ 1 * 1000 * 1000 --1 segundo
    mapM_ killThread threads
    --return responses
    gathered_responses <- readTVarIO responses
    print gathered_responses
    if (length gathered_responses) == 0 then return Nothing
    else return $ Just $ fst $ head $ sortBy (\x y -> (compare)(time $ snd x) (time $ snd y)) gathered_responses




send_probe :: UserConnection -> TVar ([(UserConnection, ProbeResponse)]) -> IO ()
send_probe uuc@(UserConnection (Just (ip, port))) responses = do
    sock <- getSockUDPClient ip port
    let datagram = fromStrict getProbeRequest
    send sock datagram
    handle <- socketToHandle sock ReadMode
    (Header t s a d) <- readHeader handle
    resto <- BS.hGet handle d 
    let tempo_after = (decode $ fromStrict resto) :: String
    atomically $ modifyTVar' responses (\k -> (uuc, (ProbeResponse {time = read tempo_after :: Double})):k) 
    return ()



