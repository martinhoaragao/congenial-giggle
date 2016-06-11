module AudioTransferHeader where
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Binary
import System.IO
import Network.Socket hiding (recv)
import Network.Socket.ByteString
import GHC.IO.Handle
import AudioTransferTypes

addHeader :: Header -> BS.ByteString -> BS.ByteString
addHeader h b = BS.concat $ [t,s,a,d,b]
  where t = toStrict $ encode $ getTipo h
        s = toStrict $ encode $ getSeqNum h
        a = toStrict $ encode $ getAckNum h
        d = toStrict $ encode $ getDataSize h

readHeader :: Handle -> IO Header
readHeader handle = do
    tp <- BS.hGet handle 1
    sq <- BS.hGet handle 8
    ak <- BS.hGet handle 8
    dt <- BS.hGet handle 8
    let t = (decode $ fromStrict tp) :: Char
        s = (decode $ fromStrict sq) :: Int
        a = (decode $ fromStrict ak) :: Int
        d = (decode $ fromStrict dt) :: Int
    return (Header t s a d)

getMessageType :: Header -> MessageType
getMessageType h = case getTipo h of
                     '1' -> REGISTER
                     '2' -> CONSULT_REQUEST
                     '3' -> CONSULT_RESPONSE
                     '4' -> PROBE_REQUEST
                     '5' -> PROBE_RESPONSE
                     '6' -> REQUEST
                     '7' -> DATA

readHeaderS :: Socket -> IO Header
readHeaderS socket = do
  b <- recv socket headerSize
  return (bs2header b)

readHeaderB :: BS.ByteString -> (Header, BS.ByteString)
readHeaderB b = (bs2header b, bs)
  where (_,bs) = BS.splitAt headerSize b

bs2header :: BS.ByteString -> Header
bs2header b = let   t = (decode $ fromStrict w) :: Char
                    s = (decode $ fromStrict x) :: Int
                    a = (decode $ fromStrict y) :: Int
                    d = (decode $ fromStrict (BS.take 8 z)) :: Int
              in (Header t s a d)
    where (w, aux1) = BS.splitAt 1 b
          (x, aux2) = BS.splitAt 8 aux1
          (y, z) = BS.splitAt 8 aux2

headerToString :: Header -> IO()
headerToString header = putStrLn $ "Tipo: "++ show (getMessageType header) ++ " SeqNum: "++ show (getSeqNum header) ++ " AckNum: "++ show (getAckNum header) ++ " DataSize: "++ show (getDataSize header)

test = do
    file <- BS.readFile "sample.txt"
    let l = BS.length file
    let packet = addHeader (Header '1' 3 20 l) file
    BS.writeFile "packet.txt" packet

    handle <- openFile "packet.txt" ReadMode
    header <- readHeader handle
    headerToString header
