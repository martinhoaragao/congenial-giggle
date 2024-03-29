module ConsultsResponses where

import           Control.Concurrent.STM
import qualified Data.Map               as DM
import           Data.Maybe

newtype ConsultsResponses = ConsultsResponses (TVar (DM.Map String [(String, String)]))


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

addConsultResponse consultsResponses username addr = atomically $
  addConsultResponseSTM username addr consultsResponses

addConsultResponseSTM :: String -> (String, String) -> ConsultsResponses -> STM ()
addConsultResponseSTM username sockAddr (ConsultsResponses consultsResponsesSTM) = do
  consultsResponses <- readTVar consultsResponsesSTM
  let consultResponses = fromJust $ DM.lookup username consultsResponses
  writeTVar consultsResponsesSTM $ DM.insert username (sockAddr : consultResponses) consultsResponses

-- Deliver Data

getConsultResponses username consultsResponses = atomically $ consultResponseSTM username consultsResponses

consultResponseSTM :: String -> ConsultsResponses -> STM [(String, String)]
consultResponseSTM username (ConsultsResponses consultsResponsesSTM) = do
  consultsResponses <- readTVar consultsResponsesSTM
  let ok = fromJust $ DM.lookup username consultsResponses
  return ok


-- Remove
removeConsultSTM :: String -> ConsultsResponses -> STM ()
removeConsultSTM username (ConsultsResponses consultsResponsesSTM) = do
  consultsResponses <- readTVar consultsResponsesSTM
  writeTVar consultsResponsesSTM $ DM.delete username consultsResponses
