module EventMgr (EventMgr,addEvent,getDue,eventNames,emptyMgr) where
import Data.Time.Clock
import Util
import Data.List (sortBy,partition)
import Data.Unique
import Data.Ord (comparing)
import Control.Monad (liftM)

type EvtID = BString
data EvtTime = ETime UTCTime | EIdle deriving (Eq)
data Event a = Evt { evtTime :: EvtTime, 
                     evtID :: EvtID, 
	                 evtAction :: a }

instance Ord EvtTime where
   compare (ETime t1) (ETime t2) = compare t1 t2
   compare (ETime _) EIdle = LT
   compare EIdle (ETime _) = GT
   compare EIdle EIdle     = EQ

newtype EventMgr a = EventMgr [Event a]


emptyMgr = EventMgr []
mgrInsert evt (EventMgr evts) = EventMgr (sortBy (comparing evtTime) (evt:evts))  

getNextID :: IO EvtID
getNextID = do
  num <- liftM hashUnique newUnique
  return $! pack ("after#" ++ show num)

eventNames (EventMgr evts) = map evtID evts 
  
addEvent act deadline el = do
  id <- getNextID
  return $ (id, mgrInsert (Evt { evtTime = toETime deadline, evtID = id, evtAction = act }) el)
 where toETime Nothing = EIdle
       toETime (Just dl) = ETime dl

notAfter now e = evtTime e <= (ETime now)
isIdle e = evtTime e == EIdle

getDue (EventMgr evts) = do
  currTime <- getCurrentTime
  let (due,rest) = partition (notAfter currTime) evts
  if null due 
      then do
        let (idles,others) = partition isIdle rest
        return (map evtAction idles, EventMgr others)
      else return (map evtAction due, EventMgr rest)
