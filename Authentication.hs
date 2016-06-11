module Authentication where

import           Control.Concurrent.STM
import           Control.Monad
import qualified Data.Map               as DM
import           Data.Maybe
import           Network.Socket         (Socket)

data User = User { getUsername   :: String
                 , getPassword   :: String
                 , getConnection :: Maybe Socket
                 } deriving (Eq, Show)

newtype Users = Users (TVar (DM.Map String User))
newtype UserConnected = UserConnected (TVar (Maybe String, Socket))

-- Helpers

connectedUsername = fromJust . fst


-- General STM

isPassValid username password (Users usersSTM) = do
  users <- readTVar usersSTM
  let user = DM.lookup username users
  let res = isJust user && (password == (getPassword . fromJust $ user))
  return res

-- Begin Data

newUsers :: IO Users
newUsers = do
  t <- newTVarIO DM.empty
  return (Users t)

newUserConnected :: Socket -> IO UserConnected
newUserConnected socket = do
  t <- newTVarIO (Nothing, socket)
  return (UserConnected t)

-- Register

registerSTM :: User -> Users -> STM ()
registerSTM user (Users usersSTM) = do
  users <- readTVar usersSTM
  writeTVar usersSTM $ DM.insert (getUsername user) user users

register username password users =
  atomically $ registerSTM (User username password Nothing) users

isRegistered username (Users usersSTM) = do
  users <- readTVar usersSTM
  let user = DM.lookup username users
  return $ isJust user

-- Log In

logInUserSTM username (UserConnected userConnSTM) (Users usersSTM) = do
  userConnected <- readTVar userConnSTM
  writeTVar userConnSTM (Just username, snd userConnected)
  users <- readTVar usersSTM
  let userOld = fromJust $ DM.lookup username users
  let userNew = User username (getPassword userOld) (Just $ snd userConnected)
  writeTVar usersSTM $ DM.insert username userNew users

isLoggedInSTM :: String -> Users -> STM Bool
isLoggedInSTM username (Users usersSTM) = do
  users <- readTVar usersSTM
  let user = DM.lookup username users
  let res = isJust user && (isJust . getConnection . fromJust $ user)
  return res

logInUser username password userConnected users = atomically $ do
  userLogged <- isLoggedInSTM username users
  passwordValid <- isPassValid username password users
  let canLogin = not userLogged && passwordValid
  when canLogin (logInUserSTM username userConnected users)

isLoggedIn (UserConnected userConnectedSTM) users = atomically $ do
  userConnected <- readTVar userConnectedSTM
  return $ isJust . fst $ userConnected

-- Log Out

logOutUser (UserConnected userConnSTM) (Users usersSTM) = atomically $ do
  userConnected <- readTVar userConnSTM
  let username = fromJust . fst $ userConnected
  writeTVar userConnSTM (Nothing, snd userConnected)
  users <- readTVar usersSTM
  let userOld = fromJust $ DM.lookup username users
  let userNew = User username (getPassword userOld) Nothing
  writeTVar usersSTM $ DM.insert username userNew users

-- Delete

deleteUser (UserConnected userSTM) (Users usersSTM) = atomically $ do
  user <- readTVar userSTM
  users <- readTVar usersSTM
  when (isJust . fst $ user) $ do
    writeTVar usersSTM $ DM.delete (connectedUsername user) users
    writeTVar userSTM (Nothing, snd user)

-- Convert Data

getConnectedUsername (UserConnected userSTM) = atomically $ do
  user <- readTVar userSTM
  return $ connectedUsername user

getConnectedUsersAddr users = do
  usersList <- atomically $ getUsersSTM users
  return $ map (fromJust . getConnection) $ filter (isJust . getConnection) usersList

getUsersSTM :: Users -> STM [User]
getUsersSTM (Users usersSTM) = do
  users <- readTVar usersSTM
  return $ DM.elems users
