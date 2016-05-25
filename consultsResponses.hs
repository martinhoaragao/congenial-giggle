module ConsultsResponses where

import           Control.Concurrent.STM
import qualified Data.Map               as DM
import           Data.Maybe
import           Network.Socket         (SockAddr)

newtype ConsultsResponses = ConsultsResponses (TVar (DM.Map String [SockAddr]))


-- Begin Data

newConsultsResponses :: IO ConsultsResponses
newConsultsResponses = do
  t <- newTVarIO DM.empty
  return (ConsultsResponses t)


-- Add

addConsult username consultsResponses =
  atomically $ addConsultSTM username consultsResponses

addConsultSTM :: String -> ConsultsResponses -> STM ()
addConsultSTM username (ConsultsResponses consultsResponsesSTM) = do
  consultsResponses <- readTVar consultsResponsesSTM
  writeTVar consultsResponsesSTM $ DM.insert username [] consultsResponses

addConsultResponse username sockAddr consultsResponses =
  atomically $ addConsultResponseSTM username sockAddr consultsResponses

addConsultResponseSTM :: String -> SockAddr -> ConsultsResponses -> STM ()
addConsultResponseSTM username sockAddr (ConsultsResponses consultsResponsesSTM) = do
  consultsResponses <- readTVar consultsResponsesSTM
  let consultResponses = fromJust $ DM.lookup username consultsResponses
  writeTVar consultsResponsesSTM $ DM.insert username (sockAddr : consultResponses) consultsResponses


-- Deliver Data

getConsultResponses username consultsResponses = atomically $ do
  removeConsultSTM username consultsResponses
  consultResponseSTM username consultsResponses

consultResponseSTM :: String -> ConsultsResponses -> STM [SockAddr]
consultResponseSTM username (ConsultsResponses consultsResponsesSTM) = do
  consultsResponses <- readTVar consultsResponsesSTM
  return $ fromJust $ DM.lookup username consultsResponses

-- Remove
removeConsultSTM :: String -> ConsultsResponses -> STM ()
removeConsultSTM username (ConsultsResponses consultsResponsesSTM) = do
  consultsResponses <- readTVar consultsResponsesSTM
  writeTVar consultsResponsesSTM $ DM.delete username consultsResponses
