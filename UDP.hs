module UDP where
import qualified Data.ByteString as BSL
import Data.ByteString.Lazy (fromStrict, toStrict)
import GHC.IO.Handle
import Control.Monad
import Data.Binary
import System.IO
import Network.Socket
import Network.Socket.ByteString as NSB
import Network.BSD
import System.Directory

import AudioTransferTypes



getType = undefined
toConsultRequest = undefined
readAll = undefined



getSockUDPClient :: HostName -> String -> IO Socket     
getSockUDPClient hostname port =  withSocketsDo $ do
 addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
 let serveraddr = Prelude.head addrinfos
 sock <- socket (addrFamily serveraddr) Datagram defaultProtocol 
 connect sock (addrAddress serveraddr)
 return sock

getSockUDPServer :: IO Socket     
getSockUDPServer = withSocketsDo $ do
 addrinfos <- getAddrInfo (Just (defaultHints {addrFlags=[AI_PASSIVE]})) Nothing (Just defined_port)
 let serveraddr = Prelude.head addrinfos
 sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
 setSocketOption sock ReuseAddr 1
 bind sock (addrAddress serveraddr)
 return sock



testSend = withSocketsDo $ do
    sock <- getSockUDPClient "localhost" defined_port --destino
    h <- socketToHandle sock ReadWriteMode 
    --sendFile ("in.txt") h
    putStrLn "Sending!"
    --sendAudio ("in2.txt") h
    --sendAudio ("000001.mp3") h
    sendAudio ("teste.mp3") h
    hClose h
    System.IO.putStrLn "Sent bytes!"


testReceive = withSocketsDo $ do
    sock <- getSockUDPServer
    h <- socketToHandle sock ReadWriteMode
    input <- getAudio ("out.m4a") h
    --getFile ("out.txt") h 
    --input <- BSL.hGet h 83
    BSL.writeFile "out.mp3" input
    System.IO.putStrLn "Received and saved file!"
{-
main = do
    k <- getChar
    if k == '1' then testReceive
        else testSend
-}
-- //////////////////////////////////////////////////// actual code below





sendAudio :: FilePath -> Handle -> IO()
sendAudio file handle = do
    f <- BSL.readFile file
    let l = BSL.length f
    let nPackets = ceiling((fromIntegral l) / (fromIntegral dataSize))
    let n = 8 --trabalhamos com int64 por enquant
    BSL.hPut handle $ toStrict (encode (l::Int))
    putStrLn("Sent nPackets: " ++ (show nPackets) ++ "\nSent bytes: " ++ (show l) );
    hFlush handle
    --BSL.hPut handle f
    let list = separate f
    mapM_ (sendDatagram handle) list
    putStrLn("Sent data");
    --hFlush handle

getAudio :: FilePath -> Handle -> IO BSL.ByteString
getAudio file handle = do
    n <- BSL.hGet handle 8
    let size = (decode $ fromStrict n) ::Int
    let nPackets = ceiling((fromIntegral size) / (fromIntegral dataSize))
    putStrLn ("Getting " ++ show(size) ++ " bytes!\nGetting " ++ show(nPackets) ++ " packets!")
    --audio_file <- BSL.hGet handle size
    almostAllPackets <- getPackets handle (nPackets-1)
    putStrLn ("Got it almost all!")
    lastPacket <- BSL.hGet handle (size - ((nPackets-1) * packetSize))
    putStrLn ("Got Last packet...")
    let return_val = (BSL.concat (almostAllPackets ++ [lastPacket]))
    putStrLn ("Data size: " ++ (show (BSL.length return_val)))
    return return_val

getPackets :: Handle -> Int -> IO [BSL.ByteString]
getPackets handle 0 = return []
getPackets handle n | n <= packetsPerRead = do
    putStrLn ("Got packets no. 0 - " ++ (show n))
    h <- BSL.hGet handle (packetSize*n)
    return ([h])
getPackets handle n | n > packetsPerRead = do
    t <- getPackets handle (n-packetsPerRead)
    putStrLn ("Got packets no. " ++ (show (n-packetsPerRead)) ++ " - " ++ (show n))
    h <- BSL.hGet handle (packetSize*packetsPerRead)
    return (t++[h])


sendFile :: FilePath -> Handle -> IO ()
sendFile file handle = do
    f <- BSL.readFile file
    let list = separate f
    --mapM_ (sendDatagram handle) list
    sendDatagram handle f

sendDatagram :: Handle -> BSL.ByteString -> IO ()
sendDatagram handle packet = do
    BSL.hPut handle packet
    hFlush handle

separate :: BSL.ByteString -> [BSL.ByteString]
separate l = separate' (fromIntegral packetSize) l
 where separate' n l = if BSL.null l then [] else (a):(separate' n b)
        where (a,b) = BSL.splitAt n l

getFile :: FilePath -> Handle -> IO ()
getFile file handle = do
    audio_file <- readAll handle
    --audio_file <- BSL.hGetContents handle
    let final_audio = reconstruct audio_file
    BSL.writeFile file final_audio

reconstruct = BSL.concat

{-readAll :: Socket -> IO [BSL.ByteString]
readAll sockReceive sockSend = do
    System.IO.putStrLn "read one"
    --p <- getDatagramHeader h
    (p, sa@(SockAddrInet port host)) <- recvfrom sockReceive headerSize 
    case getType p of 
        CONSULT_REQUEST -> do
            k <- getCurrentDirectory
            l <- getDirectoryContents k
            let file_name = fileName $ (toConsultRequest p)
            let found = elem file_name l
            let datagram = ConsultResponse { wasFound=found, numberHosts =1, userUDPConnection = UserConnection (Just (host, defined_port)) }
            let bytes = toConsultResponseByteString datagram
            sendTo sockSend bytes sa
        CONSULT_RESPONSE -> undefined
        PROBE_REQUEST -> undefined
        PROBE_RESPONSE -> undefined
        REQUEST -> undefined
        DATA -> undefined
        --dados -> readAll h
        --mensagem de fim de envios -> if (completo) then return else pedir retransmissao
    t <- readAll h
    return (p:t)
-}


getDatagram :: Handle -> IO BSL.ByteString
getDatagram handle = do
    BSL.hGet handle packetSize

getDatagramHeader :: Handle -> IO BSL.ByteString
getDatagramHeader handle = do
    BSL.hGet handle headerSize