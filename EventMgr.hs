module EventMgr (EventMgr,addEvent,getDue, emptyMgr) where
import Data.Time.Clock
import Util
import Data.List (sortBy,partition)
import Data.Unique
import Data.Ord (comparing)
import Control.Monad (liftM)

type EvtID = BString
data Event a = Evt { evtTime :: UTCTime, 
                           evtID :: EvtID, 
	                   evtAction :: a }

newtype EventMgr a = EventMgr [Event a]

emptyMgr = EventMgr []
mgrInsert evt (EventMgr evts) = EventMgr (sortBy (comparing evtTime) (evt:evts))  

getNextID :: IO EvtID
getNextID = do
  num <- liftM hashUnique newUnique
  return $! pack ("after#" ++ show num)
  
addEvent act deadline el = do
  id <- getNextID
  return $ (id, mgrInsert (Evt { evtTime = deadline, evtID = id, evtAction = act }) el)

getDue (EventMgr evts) = do
  currTime <- getCurrentTime
  let (due,rest) = partition (\x -> evtTime x <= currTime) evts
  return (map evtAction due, EventMgr rest)
