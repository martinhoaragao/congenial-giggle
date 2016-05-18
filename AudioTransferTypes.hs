module AudioTransferTypes where
import Data.Time.LocalTime

data RegisterState = LogIn | LogOut
    deriving (Show)

data UserConnection = Maybe { ip       :: String
                            , port     :: String
                            } deriving (Show)

data Register = Register { registerState     :: RegisterState
                         , userTPCConnection :: UserConnection
                         } deriving (Show)

data ConsultRequest = ConsultRequest { group    :: String
                                     , fileName :: String
                                     } deriving (Show)

data ConsultResponse = ConsultResponse { wasFound          :: Bool
                                       , numberHosts       :: Int
                                       , userUDPConnection :: UserConnection
                                       } deriving (Show)

data ProbeResponse = ProbeResponse { time :: TimeOfDay
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
                    deriving (Eq, Ord, Show, Read, Enum)

data Header = Header { getTipo :: Char
                     , getSeqNum :: Int
                     , getAckNum :: Int
                     , getDataSize :: Int
                     }
