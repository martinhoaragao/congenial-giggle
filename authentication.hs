import qualified Data.Map as DM
import Data.Maybe
import Control.Concurrent.STM

data User = User { username    :: String
                 , password    :: String
                 , isConnected :: Bool
                 } deriving (Eq, Ord, Show)


newtype Users = Users (TVar (DM.Map String User))


registerSTM :: User -> Users -> STM ()
registerSTM user (Users usersSTM) = do
  users <- readTVar usersSTM
  writeTVar usersSTM $ DM.insert (username user) user users

isLoggedIn username (Users usersSTM) = do
  users <- readTVar usersSTM
  let user = DM.lookup username users
  let res = not (isNothing user) && (isConnected . fromJust $ user)
  return res

newUsersSTM :: IO (Users)
newUsersSTM = do
  t <- newTVarIO DM.empty
  return (Users t)

main :: IO ()
main = do
  users <- newUsersSTM
  atomically $ registerSTM (User "martinho" "soufixe" False) users
  atomically $ registerSTM (User "rego" "naosoufixe" False) users
  res <- atomically $ isLoggedIn "martinho" users
  putStrLn $ show res
  putStrLn "Hello World"
