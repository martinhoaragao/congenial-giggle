module AudioTransferHeader where
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Binary
import System.IO
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

test = do
    file <- BS.readFile "sample.txt"
    let l = BS.length file
    let packet = addHeader (Header '1' 3 20 l) file
    BS.writeFile "packet.txt" packet

    handle <- openFile "packet.txt" ReadMode

    header <- readHeader handle

    putStrLn $ "Tipo: "++ show (getMessageType header) ++ " SeqNum: "++ show (getSeqNum header) ++ " AckNum: "++ show (getSeqNum header) ++ " DataSize: "++ show (getDataSize header)

