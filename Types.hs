module AudioTransferTypes where

packetSize :: Int
packetSize = headerSize + dataSize

dataSize :: Int
dataSize = 48

headerSize :: Int
headerSize = 25

packetsPerRead :: Int
packetsPerRead = 1000

defined_port :: String
defined_port = "4242"


data RegisterState = LogIn | LogOut
    deriving (Show)

data UserConnection = UserConnection {ip_port :: Maybe (String, String)
                            } deriving (Show)

data Register = Register { registerState     :: RegisterState
                         , userTPCConnection :: UserConnection
                         } deriving (Show)

data ConsultRequest = ConsultRequest { group    :: String
                                     , fileName :: String
                                     } deriving (Show)

data ConsultResponse = ConsultResponse { wasFound          :: Bool
                                       , numberHosts       :: Int
                                       , userUDPConnections :: [UserConnection]
                                       } deriving (Show)

data ProbeResponse = ProbeResponse { time :: Double
                                   } deriving (Show)

{- | Types of message. -}
data MessageType =
            REGISTER                  -- ^ Register Request
          | CONSULT_REQUEST           -- ^ Request for Music File
          | CONSULT_RESPONSE          -- ^ Whether file exists
          | PROBE_REQUEST             -- ^ Probe Transfer Rates
          | PROBE_RESPONSE            -- ^ Transfer rate test
          | REQUEST                   -- ^ Request start of file transfer
          | DATA                      -- ^ File transfer
          | ACK                       -- ^ Acknowledge
          | SYN
                    deriving (Eq, Ord, Show, Read, Enum)

data Header = Header { getTipo :: Char
                     , getSeqNum :: Int
                     , getAckNum :: Int
                     , getDataSize :: Int
                     }
