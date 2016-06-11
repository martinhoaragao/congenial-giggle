module Probe where
import           Control.Concurrent             (forkIO, killThread,
                                                 threadDelay)
import           Control.Concurrent.STM
import           Control.Monad                  (forever)
import           Data.Binary
import qualified Data.ByteString                as BS
import           Data.ByteString.Lazy           (fromStrict, toStrict, cycle, take)
import           Data.List                      (sortBy)
import           Data.Time.Clock.POSIX
import           Network.Socket                 (socketToHandle)
import           Network.Socket                 (SockAddr (SockAddrInet, SockAddrInet6),
                                                 Socket)
import           Network.Socket.ByteString      (recvFrom, sendTo)
import           Network.Socket.ByteString.Lazy
import           System.IO
import           Prelude hiding (cycle)

import           Header
import           Types
import           UDP

trash n = toStrict $ Data.ByteString.Lazy.take (128 - (fromIntegral n)) $ cycle $ encode 'a'

--Asks target for a file named file_name
send_file_request :: String -> String -> String -> IO ()
send_file_request file_name ip port = do
    sock <- getSockUDPClient ip port
    let h = Header {getTipo ='6', getSeqNum = -1, getAckNum = -1, getDataSize = BS.length $ toStrict $ encode file_name}
    let datagram = addHeader h (toStrict $ encode file_name)
    --let datagram = addHeader h (toStrict $ encode 'a')
    let datagramPadded = BS.take 128 $ BS.append datagram (trash (BS.length datagram))
    k <- send sock $ fromStrict datagramPadded
    putStr ("Asking for " ++ file_name)
    putStrLn( " of size " ++ show (k))
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
    let datagramPadded = BS.take 128 $ BS.append datagram (trash (BS.length datagram))

    sendTo sock datagramPadded sa
    --send sock $ fromStrict datagram
    return ()


 --Must run at all times on client. Will respond to Probe Requests and File Requests
udp_handler :: String -> IO ()
udp_handler udpPort = do
    sockServ <- getSockUDPServer udpPort
    forever $ do
        --(dados, sa) <- recvFrom sockServ 25
        (dados, sa) <- recvFrom sockServ 128
        putStrLn $ ("Recebido dados! Vindos de " ++ (show sa))
        let (SockAddrInet port host) = sa
        let h@(Header t s a d) = bs2header dados
        case getMessageType h of
            PROBE_REQUEST -> send_probe_response sockServ sa
            REQUEST -> do
                let file_name = BS.take d $ BS.drop 25 dados
                putStrLn $ "asked for a file " ++ (decode $ fromStrict file_name) ++ " what to heckl"
                --send_func sa file_name



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
    hClose handle
    let tempo_after = (decode $ fromStrict resto) :: String
    atomically $ modifyTVar' responses (\k -> (uuc, (ProbeResponse {time = read tempo_after :: Double})):k)
    return ()
