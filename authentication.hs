module Authentication where

import qualified Data.Map as DM
import Data.Maybe
import Control.Monad
import Control.Concurrent.STM

data User = User { getUsername :: String
                 , getPassword :: String
                 , isConnected :: Bool
                 } deriving (Eq, Ord, Show)

newtype Users = Users (TVar (DM.Map String User))
newtype UserConnected = UserConnected (TVar (Maybe String))

registerSTM :: User -> Users -> STM ()
registerSTM user (Users usersSTM) = do
  users <- readTVar usersSTM
  writeTVar usersSTM $ DM.insert (getUsername user) user users

deleteUser (UserConnected userSTM) (Users usersSTM) = atomically $ do
  user <- readTVar userSTM
  users <- readTVar usersSTM
  when (isJust user) $ do
    writeTVar usersSTM $ DM.delete (fromJust user) users
    writeTVar userSTM Nothing

register username password users =
    atomically $ registerSTM (User username password False) users

isRegistered username (Users usersSTM) = do
  users <- readTVar usersSTM
  let user = DM.lookup username users
  return $ isJust user

isLoggedInSTM :: String -> Users -> STM Bool
isLoggedInSTM username (Users usersSTM) = do
  users <- readTVar usersSTM
  let user = DM.lookup username users
  let res = isJust user && (isConnected . fromJust $ user)
  return res

isPassValid username password (Users usersSTM) = do
  users <- readTVar usersSTM
  let user = DM.lookup username users
  let res = isJust user && (password == (getPassword . fromJust $ user))
  return res

switchUserLogStatus username (Users usersSTM) = do
  users <- readTVar usersSTM
  let user = fromJust $ DM.lookup username users
  let loggedUser =  User username (getPassword user) (not (isConnected user))
  writeTVar usersSTM $ DM.insert username loggedUser users

logInUserSTM username (UserConnected userConnected) users = do
  switchUserLogStatus username users
  writeTVar userConnected $ Just username

logInUser username password userConnected users = atomically $ do
  userLogged <- isLoggedInSTM username users
  passwordValid <- isPassValid username password users
  let canLogin = not userLogged && passwordValid
  when canLogin (logInUserSTM username userConnected users)

logOutUser (UserConnected userSTM) users = atomically $ do
  user <- readTVar userSTM
  when (isJust user) $ do
    let username = fromJust user
    userLogStatus <- isLoggedInSTM username users
    switchUserLogStatus (fromJust user) users
    writeTVar userSTM Nothing

newUsers :: IO Users
newUsers = do
  t <- newTVarIO DM.empty
  return (Users t)

newUserConnected :: IO UserConnected
newUserConnected = do
  t <- newTVarIO Nothing
  return (UserConnected t)

isLoggedIn (UserConnected userConnectedSTM) = do
  userConnected <- readTVar userConnectedSTM
  return $ isJust userConnected
