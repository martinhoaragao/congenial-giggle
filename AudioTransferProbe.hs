import AudioTransferHeader
import AudioTransferTypes
import UDP
import Network.Socket (socketToHandle)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Network.Socket.ByteString.Lazy
import qualified Data.ByteString as BS
import Control.Concurrent.STM
import Control.Concurrent (forkIO, killThread, threadDelay)
import Data.Binary
import System.IO 
import Data.List (sortBy)


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



