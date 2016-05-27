module AudioTransferHeader where
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Binary
import System.IO
import GHC.IO.Handle
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString
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

readHeaderS :: Socket -> IO Header
readHeaderS socket = do
  h <- recv socket 25
  let t = (decode $ fromStrict (BS.take 1 h)) :: Char
      x = BS.drop 1 h
      s = (decode $ fromStrict (BS.take 8 x)) :: Int
      y = BS.drop 8 x
      a = (decode $ fromStrict (BS.take 8 y)) :: Int
      z = BS.drop 8 y
      d = (decode $ fromStrict (BS.take 8 z)) :: Int
  return (Header t s a d)

readHeaderB :: BS.ByteString -> (Header, BS.ByteString)
readHeaderB packet = (bsToHeader (BS.take headerSize packet), BS.drop headerSize packet)

bsToHeader :: BS.ByteString -> Header
bsToHeader b = let t = (decode $ fromStrict (BS.take 1 b)) :: Char
                   x = BS.drop 1 b
                   s = (decode $ fromStrict (BS.take 8 x)) :: Int
                   y = BS.drop 8 x
                   a = (decode $ fromStrict (BS.take 8 y)) :: Int
                   z = BS.drop 8 y
                   d = (decode $ fromStrict (BS.take 8 z)) :: Int
               in (Header t s a d)

getMessageType :: Header -> MessageType
getMessageType h = case getTipo h of
                     '1' -> REGISTER
                     '2' -> CONSULT_REQUEST
                     '3' -> CONSULT_RESPONSE
                     '4' -> PROBE_REQUEST
                     '5' -> PROBE_RESPONSE
                     '6' -> REQUEST
                     '7' -> DATA
                     '8' -> ACK
                     '9' -> SYN

headerToString :: Header -> IO()
headerToString header = putStrLn $ "Tipo: "++ show (getMessageType header) ++ " SeqNum: "++ show (getSeqNum header) ++ " AckNum: "++ show (getAckNum header) ++ " DataSize: "++ show (getDataSize header)


test = do
  file <- BS.readFile "sample.txt"
  let l = BS.length file
  let packet = addHeader (Header '7' 3 20 l) file
  BS.writeFile "packet.txt" packet

  handle <- openFile "packet.txt" ReadMode

  header <- readHeader handle
  headerToString header
