module UDP where
import           Data.Binary
import qualified Data.ByteString           as BS
import           Data.ByteString.Lazy      (fromStrict, toStrict)
import           Data.List
import           Network.BSD
import           Network.Socket            hiding (recv, recvFrom, send, sendTo)
import           Network.Socket.ByteString as NSB
import           System.Random
import           System.Timeout

import           Header
import           Types


getSockUDPClient :: HostName -> String -> IO Socket
getSockUDPClient hostname port =  withSocketsDo $ do
  addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
  let serveraddr = Prelude.head addrinfos
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  connect sock (addrAddress serveraddr)
  return sock

getSockUDPServer :: String -> IO Socket
getSockUDPServer udpPort = withSocketsDo $ do
  addrinfos <- getAddrInfo (Just (defaultHints {addrFlags=[AI_PASSIVE]})) Nothing (Just udpPort)
  let serveraddr = Prelude.head addrinfos
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress serveraddr)
  return sock

-- Retransmission (Inteiros - 8 bits)

separate :: BS.ByteString -> [BS.ByteString]
separate = separate' (fromIntegral dataSize)
  where separate' n l = if BS.null l then [] else a : separate' n b
          where (a,b) = BS.splitAt n l

oneList :: [a] -> [b] -> [(a,b)]
oneList _ [] = []
oneList [] _ = []
oneList (x:xs) (y:ys) = (x,y) : oneList xs ys

sortP :: (Int,BS.ByteString) -> (Int,BS.ByteString) -> Ordering
sortP (a,_) (b,_)
    | a < b = LT
    | a == b = EQ
    | otherwise = GT

sendAck :: Socket -> SockAddr -> Int -> IO()
sendAck socket addr n = withSocketsDo $ do
  let packet = addHeader (Header '8' 0 n 0) BS.empty
  bytes <- sendTo socket packet addr
  putStrLn $ "Ack sent: " ++ show n ++ " (" ++ show bytes ++ " bytes)"

readAck :: Socket -> IO Int
readAck socket = withSocketsDo $ do
  header <- readHeaderS socket
  return (getAckNum header)

readPacket :: Socket -> IO (Int,BS.ByteString)
readPacket socket = withSocketsDo $ do
  putStrLn "Reading Packet..."
  (packet, addr) <- recvFrom socket packetSize
  let (header, info) = readHeaderB packet
  --headerToString header
  putStrLn $ "Received packet: " ++ show (getSeqNum header) ++ " (" ++ show (BS.length packet) ++ " bytes)"
  sendAck socket addr 1
  return (getSeqNum header,info)

sendPacket :: Socket -> BS.ByteString -> IO()
sendPacket socket packet = withSocketsDo $ do
  r <- randomRIO(0,100) :: IO Int
  if r == 0 then
              putStrLn "Lost packet!"
            else do
              send socket packet
              putStrLn $ "Packet sent (" ++ show (BS.length packet) ++ " bytes)"
  putStrLn "Waiting for Ack..."
  time <- timeout 10000 (readAck socket)
  case time of
    Nothing -> do
      putStrLn "No Ack received... Resending..."
      sendPacket socket packet
    (Just a) ->
      if a == 1 then putStrLn "Received Ack!"
                else do
                  putStrLn "Client asked to resend!"
                  sendPacket socket packet

send_func :: SockAddr -> String -> IO()
send_func sockaddr ficheiro = withSocketsDo $ do
  (Just host, Just port) <- getNameInfo [NI_NOFQDN, NI_NUMERICHOST] True True sockaddr
  --socket <- getSockUDPClient host port
  socket <- getSockUDPClient host "10514"
  f <- BS.readFile ficheiro
  let l = BS.length f
      list = oneList [1..] (separate f)
      nPackets = length list
      lbs = toStrict (encode (l::Int))
      nbs = toStrict (encode (nPackets::Int))
      headerL = Header '7' (-1) 0 (BS.length lbs)
      headerN = Header '7' 0 0 (BS.length nbs)
  headerToString headerL
  sendPacket socket (addHeader headerL lbs)
  headerToString headerN
  sendPacket socket (addHeader headerN nbs)

  let p = map (\(i,s) -> (addHeader (Header '7' i 0 dataSize) s)) list
  mapM_ (sendPacket socket) p

  putStrLn "File sent!"

  close socket

recv_func :: String -> String -> IO()
recv_func ficheiro port = withSocketsDo $ do
  --socket <- getSockUDPServer port
  socket <- getSockUDPServer "10514"
  (_,lPacket) <- readPacket socket
  let l = (decode $ fromStrict lPacket) ::Int
  (_,nPacket) <- readPacket socket
  let n = (decode $ fromStrict nPacket) ::Int

  list <- mapM (\i -> readPacket socket) [1..n]
  let dt = sortBy sortP list
      d = BS.concat (map snd dt)
  BS.writeFile "out.mp3" d
  putStrLn "Received and saved file!"

  putStrLn $ "Data: " ++ show n ++ " packets (" ++ show l ++ " bytes)"
  close socket

testSend = withSocketsDo $ do
  socket <- getSockUDPClient "localhost" "10513"
  f <- BS.readFile "test.mp3"
  let l = BS.length f
      list = oneList [1..] (separate f)
      nPackets = length list
      lbs = toStrict (encode (l::Int))
      nbs = toStrict (encode (nPackets::Int))
      headerL = Header '7' (-1) 0 (BS.length lbs)
      headerN = Header '7' 0 0 (BS.length nbs)
  headerToString headerL
  sendPacket socket (addHeader headerL lbs)
  headerToString headerN
  sendPacket socket (addHeader headerN nbs)

  let p = map (\(i,s) -> (addHeader (Header '7' i 0 dataSize) s)) list
  mapM_ (sendPacket socket) p

  putStrLn "File sent!"

  close socket

testReceive = withSocketsDo $ do
  socket <- getSockUDPServer "10550"
  (_,lPacket) <- readPacket socket
  let l = (decode $ fromStrict lPacket) ::Int
  (_,nPacket) <- readPacket socket
  let n = (decode $ fromStrict nPacket) ::Int

  list <- mapM (\i -> readPacket socket) [1..n]
  let dt = sortBy sortP list
      d = BS.concat (map snd dt)
  BS.writeFile "out.mp3" d
  putStrLn "Received and saved file!"

  putStrLn $ "Data: " ++ show n ++ " packets (" ++ show l ++ " bytes)"
  close socket
