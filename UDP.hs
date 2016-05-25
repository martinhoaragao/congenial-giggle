module UDP where
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromStrict, toStrict)
import GHC.IO.Handle
import Control.Monad
import Control.Concurrent (forkIO)
import Data.Binary
import Data.List
import System.IO
import Network.Socket
import Network.Socket.ByteString as NSB
import Network.BSD
import System.Directory
import System.Random

import AudioTransferTypes
import AudioTransferHeader


getType = undefined
toConsultRequest = undefined
send_probe_requests = undefined
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


-- Retransmission Test

sendAck :: Handle -> Int -> IO()
sendAck handle seqNum = do
    let packet = addHeader (Header '8' 0 seqNum 0) BS.empty
    BS.hPut handle packet
    hFlush handle
{-}
sendSyn :: Handle -> IO()
sendSyn handle = do
    let packet = addHeader (Header '9' 0 0 0) BS.empty
    BS.hPut handle packet
    hFlush handle

receiveSyn :: Handle -> IO(Maybe  MessageType)
receiveSyn h = do
    header <- readHeader h
    let dsize = getDataSize header
        msgT = getMessageType header
    if (dsize == 0 && msgT == SYN) then return (Just SYN)
                                   else return Nothing

senderHandshake :: Handle -> Handle -> IO(Int)
senderHandshake hS hC = do
    sendSyn hC
    putStrLn "Sent SYN"
    s <- receiveSyn hS
    if((s /= Nothing) && (getValue s == SYN))
    	then do
	        sendSyn hC
	        putStrLn "Received SYN + Ack"
	        putStrLn "Sent ACK"
	        return 0
    	else return 1
    where getValue (Just x) = x

receiverHandshake :: Handle -> Handle -> IO(Int)
receiverHandshake hS hC = do
    s <- receiveSyn hS
    putStrLn "Received SYN"
    if((s /= Nothing) && (getValue s == SYN))
    	then do
    		sendSyn hC
	        putStrLn "Sent SYN + ACK"
	        s <- receiveSyn hS
        	if((s /= Nothing) && (getValue s == SYN))
        		then do
	            	putStrLn "Received SYN"
	            	return 0
        		else return 1
   		else return 1
    where getValue (Just x) = x
-}

separate :: BS.ByteString -> [BS.ByteString]
separate l = separate' (fromIntegral packetSize) l
 where separate' n l = if BS.null l then [] else (a):(separate' n b)
        where (a,b) = BS.splitAt n l

oneList :: [a] -> [b] -> [(a,b)]
oneList _ [] = []
oneList [] _ = []
oneList (x:xs) (y:ys) = [(x,y)] ++ (oneList xs ys)

sendDatagram :: Handle -> BS.ByteString -> IO ()
sendDatagram handle packet = do
    BS.hPut handle packet
    hFlush handle

sendAudio :: Handle -> [(Int,BS.ByteString)] -> Int -> IO()
sendAudio _ [] _= return ()
sendAudio handle (x:xs) n = do
	let packet = addHeader (Header '7' (fst x) 0 (BS.length (snd x))) (snd x)
	r <- randomRIO (0,10) :: IO Int
	if (r /= 0) then do
					sendDatagram handle packet
					putStrLn $ "Sent packet " ++ (show (fst x)) ++ "/" ++ (show n)
		   		else putStrLn $ "Lost packet " ++ (show (fst x)) ++ "/" ++ (show n)
	sendAudio handle xs n

getAudio :: Handle -> [Int] -> IO [(Int,BS.ByteString)]
getAudio handle id = do
	l <- readHandle handle [] id
	return l

readHandle :: Handle -> [(Int,BS.ByteString)] -> [Int] -> IO [(Int,BS.ByteString)]
readHandle h l [] = do
	askRet [-1]
	return l
readHandle h l id = do
	putStrLn "--------------------------------->"
	header <- readHeader h
	putStrLn "-------------------------------------------------------->"
	bs <- BS.hGet h (getDataSize header)
	if (getSeqNum header /= 0) then putStrLn $ "Received packet " ++ (show (getSeqNum header))
						  	   else putStrLn "-----   ACK   -----"
	if (getMessageType header == DATA) then readHandle h ((getSeqNum header, bs):l) (delete (getSeqNum header) id)
									   else do
									   		askRet id
									   		readHandle h l id

askRet :: [Int] -> IO()
askRet [] = return ()
askRet id = withSocketsDo $ do
	sockC <- getSockUDPClient "localhost" defined_port
	hC <- socketToHandle sockC ReadWriteMode
	let l = length id
	BS.hPut hC $ toStrict (encode (l::Int))
	mapM_ (askList hC) id
	hClose hC
	
	where askList h x = BS.hPut h $ toStrict (encode (x::Int))

respRet :: [(Int,BS.ByteString)] -> IO()
respRet list  = withSocketsDo $ do
	sockS <- getSockUDPServer
	hS <- socketToHandle sockS ReadWriteMode
	respRetAux hS list

respRetAux :: Handle -> [(Int,BS.ByteString)] -> IO()
respRetAux hS list = withSocketsDo $ do
	putStrLn "--------------------------------->"
	s <- BS.hGet hS 8
	putStrLn "-------------------------------------------------------->"
	let l = (decode $ fromStrict s) ::Int
	if(l == -1) then return ()
				else do
					m <- mapM (respList hS) [1..l]
					sendRet (retList m list)
					hClose hS
					putStrLn "Retransmited some packets"
					respRet list
	
	where respList h x = do
		s <- BS.hGet h 8
		let l = (decode $ fromStrict s) ::Int
		return l

sendRet :: [(Int,BS.ByteString)] -> IO()
sendRet [] = return ()
sendRet l = withSocketsDo $ do
	sockC <- getSockUDPClient "localhost" defined_port
	hC <- socketToHandle sockC ReadWriteMode
	sendAudio hC l (length l)
	sendAck hC 0

retList :: [Int] -> [(Int,BS.ByteString)] -> [(Int,BS.ByteString)]
retList [] _ = []
retList _ [] = []
retList m ((x,y):ls) = if (elem x m) then (x,y):(retList m ls)
									 else retList m ls

sortP :: (Int,BS.ByteString) -> (Int,BS.ByteString) -> Ordering
sortP (a,_) (b,_)
	| a < b = LT
	| a == b = EQ 
	| otherwise = GT


testSend = withSocketsDo $ do
    sockC <- getSockUDPClient "localhost" defined_port
    hC <- socketToHandle sockC ReadWriteMode
    f <- BS.readFile "test.mp3"
    let l = BS.length f
    	list = oneList [1..] (separate f)
    	nPackets = length list
    	n = 8 --trabalhamos com int64
    BS.hPut hC $ toStrict (encode (l::Int))
    BS.hPut hC $ toStrict (encode (nPackets::Int))
    hFlush hC
    sendAudio hC list nPackets
    sendAck hC 0
    respRet list
    sendAck hC 0
    hClose hC

testReceive = withSocketsDo $ do
    sockS <- getSockUDPServer
    hS <- socketToHandle sockS ReadWriteMode
    s <- BS.hGet hS 8
    t <- BS.hGet hS 8
    let l = (decode $ fromStrict s) ::Int
    	nPackets = (decode $ fromStrict t) ::Int
    input <- getAudio hS [1..nPackets]
    let dt = sortBy sortP input
    	d = BS.concat (map (\i -> snd i) dt)
    BS.writeFile "out.mp3" d
    putStrLn "Received and saved file!"
    hClose hS
    putStrLn $ "Data size: " ++ (show (BS.length d))
    
-- End Retransmission Test

 {-
main = do
    k <- getChar
    if k == '1' then testReceive
        else testSend
-}
-- //////////////////////////////////////////////////// actual code below

{-
sendAudio :: FilePath -> Handle -> IO()
sendAudio file handle = do
    f <- BS.readFile file
    let l = BS.length f
    let nPackets = ceiling((fromIntegral l) / (fromIntegral dataSize))
    let n = 8 --trabalhamos com int64 por enquant
    BS.hPut handle $ toStrict (encode (l::Int))
    putStrLn("Sent nPackets: " ++ (show nPackets) ++ "\nSent bytes: " ++ (show l) );
    hFlush handle
    --BS.hPut handle f
    let list = separate f
    mapM_ (sendDatagram handle) list
    putStrLn("Sent data");
    --hFlush handle

getAudio :: FilePath -> Handle -> IO BS.ByteString
getAudio file handle = do
    n <- BS.hGet handle 8
    let size = (decode $ fromStrict n) ::Int
    let nPackets = ceiling((fromIntegral size) / (fromIntegral dataSize))
    putStrLn ("Getting " ++ show(size) ++ " bytes!\nGetting " ++ show(nPackets) ++ " packets!")
    --audio_file <- BS.hGet handle size
    almostAllPackets <- getPackets handle (nPackets-1)
    putStrLn ("Got it almost all!")
    lastPacket <- BS.hGet handle (size - ((nPackets-1) * packetSize))
    putStrLn ("Got Last packet...")
    let return_val = (BS.concat (almostAllPackets ++ [lastPacket]))
    putStrLn ("Data size: " ++ (show (BS.length return_val)))
    return return_val

getPackets :: Handle -> Int -> IO [BS.ByteString]
getPackets handle 0 = return []
getPackets handle n | n <= packetsPerRead = do
    putStrLn ("Got packets no. 0 - " ++ (show n))
    h <- BS.hGet handle (packetSize*n)
    return ([h])
getPackets handle n | n > packetsPerRead = do
    t <- getPackets handle (n-packetsPerRead)
    putStrLn ("Got packets no. " ++ (show (n-packetsPerRead)) ++ " - " ++ (show n))
    h <- BS.hGet handle (packetSize*packetsPerRead)
    return (t++[h])


sendFile :: FilePath -> Handle -> IO ()
sendFile file handle = do
    f <- BS.readFile file
    let list = separate f
    --mapM_ (sendDatagram handle) list
    sendDatagram handle f

getFile :: FilePath -> Handle -> IO ()
getFile file handle = do
    audio_file <- readAll handle
    --audio_file <- BS.hGetContents handle
    let final_audio = reconstruct audio_file
    BS.writeFile file final_audio

reconstruct = BS.concat

{-readAll :: Socket -> IO [BS.ByteString]
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


getDatagram :: Handle -> IO BS.ByteString
getDatagram handle = do
    BS.hGet handle packetSize

getDatagramHeader :: Handle -> IO BS.ByteString
getDatagramHeader handle = do
    BS.hGet handle headerSize
-}