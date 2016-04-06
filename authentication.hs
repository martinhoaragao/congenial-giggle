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

registerSTM :: User -> Users -> STM ()
registerSTM user (Users usersSTM) = do
  users <- readTVar usersSTM
  writeTVar usersSTM $ DM.insert (getUsername user) user users

register username password users =
    atomically $ registerSTM (User username password False) users

isRegistered username (Users usersSTM) = do
  users <- readTVar usersSTM
  let user = DM.lookup username users
  return $ isJust user

isLoggedIn username (Users usersSTM) = do
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

logInUser username password users = atomically $ do
  userLogged <- isLoggedIn username users
  passwordValid <- isPassValid username password users
  let canLogin = not userLogged && passwordValid
  when canLogin (switchUserLogStatus username users)

logOutUser username users = atomically $ do
  userLogStatus <- isLoggedIn username users
  when userLogStatus (switchUserLogStatus username users)

newUsersSTM :: IO Users
newUsersSTM = do
  t <- newTVarIO DM.empty
  return (Users t)
