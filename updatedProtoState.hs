module ProtoState where

import Prelude hiding (lookup)
import Data.Map hiding ((\\), null, foldl)
import qualified Data.Map as M
import Data.List hiding (insert, lookup)
import qualified Data.List as L

import {-qualified-} Control.Monad.Trans.State {-as T-}
import Control.Monad.Trans(liftIO)

import ParseFile


--type Id = Int --Principal Id and Slot Id the same for now
type Name = String
type MessageId = Int

type MessageText = String
type To = Name --Destination
data Contents = Send MessageText To
                | Action MessageText  --Action-send to self(vertical action bar with text beside?)--
                deriving (Show, Eq)

type Pos = Int --Represents a row(for messages) or column(for principals) position in the diagram
data Message = Message { row       :: Pos   -- Pos represents message row
                       , contents  :: Contents
                       } deriving (Eq,Show)
               
type Outbox = [MessageId]
data Principal = Principal
                 { col     :: Pos   --Pos represents principal column
                 , outbox  :: Outbox
                 } deriving (Eq, Show)


-- TODO: add messageIndex :: Map MessageId Slot
--       AND change Outbox to:  type Outbox = [MessageId] where MessageId = Int
data ProtoState = ProtoState
             { principals        :: Map Name Principal
             , messages          :: Map MessageId Message 
             , maxCol            :: Pos
             , maxRow            :: Pos  
             , maxMessageId      :: MessageId
             , freeMids          :: [MessageId]
             , editMode          :: Bool
             } deriving (Eq, Show)


type Proto = StateT ProtoState IO
data MaybeAfter = End
                  | After Pos

stateMain = exec pTest

exec :: Proto () -> IO ProtoState
exec p = execStateT p startState

outputState :: Proto ()
outputState = do
  state <- get
  liftIO $ putStrLn $ show state
  liftIO $ putStrLn ""

pTest :: Proto ()
pTest = do
  mds <- liftIO $ fromFile "testFile.txt"
  buildPrincipals mds 1
  addMessagesAt 1 mds
  outputState
  --liftIO $ print mds
  --outputState
  --removePrincipalCalled "1"
  --removePrincipalCalled "2"
  
  {-state'' <- get
  removeMessage 1
  state <- get
  liftIO $ print (state'' == state)
  removeMessage 1
  state' <- get
  liftIO $ print (state == state')
  pruneOutboxes
-}
  
  --addMessageAt 2 (MessageD "1" "2" "1 to 2")
  --addMessageAt 4 (MessageD "1" "2" "1 to 2")
  --addMessageAt 1 (MessageD "1" "2" "1 to 2")
  removeMessage 2
  pruneOutboxes
  --outputState
  removePrincipalCalled "2"
  outputState
  addMessageAt 1 (MessageD "1" "2" "1 to 2")
  outputState
  buildPrincipals [(MessageD "2" "1" "2 to 1")] 3
  addMessagesAt 1 [(MessageD "2" "1" "2 to 1")]
  buildPrincipals [(MessageD "4" "1" "4 to 1")] 4
  addMessagesAt 5 [(MessageD "4" "1" "4 to 1")]
  outputState
  removePrincipalCalled "1"
  outputState
  addMessageAt 4 (MessageD "4" "2" "4 to 2")
  outputState
  removePrincipalCalled "4"
  outputState
  removeMessage 4
  pruneOutboxes
  --removePrincipalCalled "3"
  --removePrincipalCalled "2"
  
  --pruneOutboxes



  {-mc <- getMaxCol 
  buildPrincipals mds (mc + 1)
  outputState
  removePrincipalCalled "1"-}
  
  {-newNames <- getNewNames mds
  liftIO $ mapM_ putStrLn newNames 

  addPrincipalsAt 1 ["1","4"]
  --outputState
  addPrincipalsAt 2 ["2","3"]
  --state <- get
  outputState
  --removePrincipalAt 3

  addPrincipalsAt 1 ["1","5"]
  outputState
  --state' <- get
  --liftIO $ putStrLn $ show $ state == state'
  removePrincipalCalled "1"
  removePrincipalCalled "2"
  removePrincipalCalled "4"
  removePrincipalCalled "5"
  removePrincipalCalled "3" -}
  return ()

  
pruneOutboxes :: Proto ()
pruneOutboxes = do
  ps <- getPrincipals
  ms <- getMessages
  let newPs = M.map (pruneOutbox ms) ps
  setPMap newPs
  

pruneOutbox :: Map MessageId Message -> Principal -> Principal
pruneOutbox ms (Principal col obIn) =
  --ms <- getMessages
  let maybeIds = L.map (g ms) obIn
      newOutbox = foldl f [] maybeIds in
  (Principal col newOutbox)

 where g :: Map MessageId Message -> MessageId -> (Maybe MessageId)
       g ms mid = do
         --ms <- getMessages
         case member mid ms of
           True -> Just mid
           False -> Nothing
      

       f :: Outbox -> Maybe MessageId -> Outbox
       f mids maybeId =
         case maybeId of
           Just mid -> mids ++ [mid]
           Nothing -> mids


--refersTo :: Name -> Proto [Name]
  
removeMessage :: MessageId -> Proto ()
removeMessage mid = do
  ms <- getMessages
  ps <- getPrincipals
  let maybeM = M.lookup mid ms
  case maybeM of
    Just(Message row (Send mt dest)) -> do
      --referencedBy <-
      --posIn <- getMessagePos mid
      upDownRows False row --posIn
      decrMaxRow
      ms' <- getMessages
      let newMMap = M.delete mid ms'
      setMMap newMMap
      pushFreeMid mid
      
    Nothing -> return ()

getMessagePos :: MessageId -> Proto Pos
getMessagePos mid = do
  ms <- getMessages
  let maybeM = M.lookup mid ms
  case maybeM of
    Nothing -> error "SHOULDN'T GET HERE!!" --return (-1)
    Just (Message row _) -> do return row


  
addMessagesAt :: Pos -> [MessageD] -> Proto ()
addMessagesAt posIn [] = return ()
addMessagesAt posIn (md:mds) = do


  addMessageAt posIn md
  addMessagesAt (posIn + 1) mds
          
          


addMessageAt :: Pos -> MessageD -> Proto ()
addMessageAt posIn (MessageD fr t m) = do
  newMid <- getNewMid
  upDownRows True posIn  
  addToMMap (newMid, Message posIn (Send m t))    --Self action logic HERE!!!
  incrMaxRow
  ps <- getPrincipals
  let maybeP = M.lookup fr ps
  case maybeP of
    Nothing -> return ()
    Just (Principal col outbox) -> do
      let newP = Principal col (outbox ++ [newMid])
          newPMap = M.insert fr newP ps
      setPMap newPMap
  

  

getNewMid :: Proto MessageId
getNewMid = do
  freeId <- popFreeMid
  newMid <- case freeId of
    Nothing -> do
      maxMid <- getMaxMid
      incrMaxMid
      return (maxMid + 1)
    Just fid -> return fid
  return newMid



buildPrincipals :: [MessageD] -> Pos -> Proto ()
buildPrincipals mds pos = do
  newNames <- getNewNames mds
  addPrincipalsAt pos newNames
  



getNewNames :: [MessageD] -> Proto [Name]
getNewNames mds = do
  ps <- getPrincipals
  let oldNames = keys ps
      oldPlusNewNames = Prelude.foldl f oldNames mds
      newNames = (\\) oldPlusNewNames oldNames
  return newNames

 where f :: [Name] -> MessageD -> [Name]
       f ns md = let x = from md
                     y = to md
                     res = case x `elem` ns of
                       True -> ns
                       False -> ns ++ [x]
                     res' = case y `elem` res of
                       True -> res
                       False -> res ++ [y]
                 in res'


doesPrincipalExist :: Name -> Proto Bool
doesPrincipalExist name = do
  ps <- getPrincipals
  return (member name ps)
  
addPrincipalsAt :: Pos -> [Name] -> Proto ()
addPrincipalsAt posIn [] = return ()
addPrincipalsAt posIn (name:rest) = do
    existsPrior <- doesPrincipalExist name
    case existsPrior of
         True -> addPrincipalsAt posIn rest
         False -> do
           addPrincipalAt posIn name
           addPrincipalsAt (posIn + 1) rest

addPrincipalAt :: Pos -> Name -> Proto ()
addPrincipalAt posIn name = do
  upDownCols True posIn
  addToPMap (name, Principal posIn [])
  incrMaxCol

removePrincipalCalled :: Name -> Proto ()
removePrincipalCalled name = do
  ps <- getPrincipals
  case lookup name ps of
    Nothing -> return ()
    Just (Principal col outbox) -> do
      mapM_ removeMessage outbox  --update(messages that send to principal being removed) OR remove(messages 
      removePrincipalAt col



removePrincipalAt :: Pos -> Proto ()
removePrincipalAt posIn = do
  ps <- getPrincipals
  let list = toList ps
      newList = deleteBy remPred ("", Principal posIn []) list
      newPmap = fromList newList
  setPMap newPmap
  upDownCols False posIn
  decrMaxCol
 where
   remPred :: (Name, Principal) -> (Name,Principal) -> Bool
   remPred (_, Principal col _) (_, Principal col' _) = col == col'


upDownCols :: Bool -> Pos -> Proto ()
upDownCols b posInserted = do
  ps <- getPrincipals
  let list :: [(Name, Principal)]
      list = toList ps
      updatedList = changeCols b posInserted list
  setPMap (fromList updatedList)
  return ()

changeCols :: Bool -> Pos -> [(Name, Principal)] -> [(Name, Principal)] 
changeCols b pos xs = L.map (change b pos) xs

change :: Bool -> Pos -> (Name, Principal) -> (Name, Principal)
change sign pos (name, pIn@(Principal col outbox)) =
  let plusMinus = case sign of
        True -> 1
        False -> (-1)
      newP = case col >= pos of
        True -> Principal (col + plusMinus) outbox
        False -> pIn in
  (name, newP)
  
upDownRows :: Bool -> Pos -> Proto ()
upDownRows b posInserted = do
  ms <- getMessages
  let list :: [(MessageId, Message)]
      list = toList ms
      updatedList = changeRows b posInserted list
  setMMap (fromList updatedList)
  return ()

changeRows :: Bool -> Pos -> [(MessageId, Message)] -> [(MessageId, Message)] 
changeRows b pos xs = L.map (change' b pos) xs

change' sign pos (mid, mIn@(Message row contents)) =
  let plusMinus = case sign of
        True -> 1
        False -> (-1)
      newM = case row >= pos of
        True -> Message (row + plusMinus) contents
        False -> mIn in
  (mid, newM)



getMaxCol :: Proto Pos
getMaxCol = do
  state <- get
  return $ maxCol state

getMaxMid :: Proto MessageId
getMaxMid = do
  state <- get
  return $ maxMessageId state


setPMap :: Map Name Principal -> Proto ()
setPMap newMap = do
   (ProtoState _ a b c d e f) <- get
   put (ProtoState newMap a b c d e f)

setMMap :: Map MessageId Message -> Proto ()
setMMap newMap = do
   (ProtoState a _ b c d e f) <- get
   put (ProtoState a newMap b c d e f)


addToPMap :: (Name, Principal) -> Proto ()
addToPMap (n, p) = do
  (ProtoState ps a b c d e f) <- get
  let newPs = insert n p ps
  put (ProtoState newPs a b c d e f)

--addToPMap :: (Name, Principal) -> Proto ()
addToMMap (mid, m) = do
  (ProtoState a ms c d e f g) <- get
  let newMs = insert mid m ms
  put (ProtoState a newMs c d e f g)
  

getPrincipals :: Proto (Map Name Principal)
getPrincipals = do
  state <- get
  return (principals state)

getMessages :: Proto (Map MessageId Message)
getMessages = do
  state <- get
  return (messages state)

getFreeMids :: Proto [MessageId]
getFreeMids = do
  state <- get
  return (freeMids state)

pushFreeMid :: MessageId -> Proto ()
pushFreeMid mid = do
  (ProtoState a b c d e fs g) <- get
  put (ProtoState a b c d e (fs ++ [mid]) g)

popFreeMid :: Proto (Maybe MessageId)
popFreeMid = do
  (ProtoState a b c d e fids g) <- get
  case null fids of
    True -> return Nothing
    False -> do
      put (ProtoState a b c d e (tail fids) g)
      return $ Just (head fids)

removeFreeMid :: MessageId -> Proto ()
removeFreeMid mid = do
  (ProtoState a b c d e fs g) <- get
  put (ProtoState a b c d e (L.delete mid fs) g)


incrMaxCol :: Proto ()
incrMaxCol = do
  (ProtoState a b maxCol d e f g) <- get
  put (ProtoState a b (maxCol+1) d e f g)

incrMaxRow :: Proto ()
incrMaxRow = do
  (ProtoState a b c maxRow e f g) <- get
  put (ProtoState a b c (maxRow+1) e f g)

decrMaxCol :: Proto ()
decrMaxCol = do
  (ProtoState a b maxCol d e f g) <- get
  put (ProtoState a b (maxCol-1) d e f g)

decrMaxRow :: Proto ()
decrMaxRow = do
  (ProtoState a b c maxRow e f g) <- get
  put (ProtoState a b c (maxRow - 1) e f g)

incrMaxMid :: Proto ()
incrMaxMid = do
  (ProtoState a b c d maxMid f g) <- get
  put (ProtoState a b c d (maxMid+1) f g)




{-
stateMain = exec pTest

exec :: Proto () -> IO ProtoState
exec p = T.execStateT p startState
-}

{-
addFromFile :: FilePath -> Proto ()
addFromFile fileName = do
  mds <- liftIO $ fromFile fileName
  addMessages mds
  
pTest :: Proto ()
pTest = do    --liftIO $ putStrLn "hi"
  addFromFile ""

addMessages :: [MessageD] -> Proto ()
addMessages mds = do
  buildPrincipals mds
  buildMessages mds
  return ()
--maintain only the messageId in the outbox, then maintain an overall Map Id Contents.  Then we can insert messages at arbitrary points using indexedArray.hs(in combination with the Data.Map.toList)
nameToId :: Name -> Proto Id
nameToId n = do
  s <- T.get
  let ps = principals s
      names = Prelude.map name (elems ps)
      maybeIndex = elemIndex n names
      id = case maybeIndex of
        Nothing -> -1
        Just i -> i+1
  return id


buildMessages :: [MessageD] -> Proto ()
buildMessages = mapM_ updateMailbox

updateMailbox :: MessageD -> Proto ()
updateMailbox md = do
  s@(ProtoState a b c d) <- T.get
  let oldPMap = principals s
      from' = from md
  fromId <- nameToId from'
  let maybeP = Data.Map.lookup fromId oldPMap
      oldP = case maybeP of
        Nothing -> Principal "" [] --error handling?
        Just p -> p
  newP <- update md oldP
  let newPmap = Data.Map.insert fromId newP oldPMap
      {-newP = Data.Map.map (update md s) oldP-}
  T.put (ProtoState newPmap b (c+1) d)
 where update :: MessageD -> Principal -> Proto Principal
       update md p@(Principal name outbox) = do
         --oldP = principals state
         state@(ProtoState a b c d) <- T.get
         let source = from md
         sourceId <- nameToId source
         let to' = to md
         toId <- nameToId to'
         let newMessage = message md
         pid <- nameToId name
         let newMessageId = (maxMessageId state) + 1
             newP = case sourceId == pid of
                         True -> let newContents = Send newMessage toId
                                     newSlot = (newMessageId, newContents) in
                                 Principal name (outbox ++ [newSlot])
                         False -> p in {-let newSlot = (newMessageId, Blank) in
                       Principal name (inbox ++ [newSlot]) in -}
           return newP


buildPrincipals :: [MessageD] -> Proto ()
buildPrincipals mds = do
  (ProtoState ps oldMaxPID oldMaxMID oldEM) <- T.get
  newNames <- getNewNamesP mds
  let newPpairs = zipWith f [(oldMaxPID + 1)..] newNames
      newPMap = Data.Map.union ps (fromList newPpairs)
    in T.put (ProtoState newPMap (oldMaxPID + length newNames) oldMaxMID oldEM)
 where f :: Int -> Name -> (Int, Principal)
       f id n = (id, Principal n [])


getNewNamesP :: [MessageD] -> Proto [Name]
getNewNamesP mds = do
  oldState <- T.get
  let oldPMap = principals oldState
      oldNames = Prelude.map name (elems oldPMap)
      oldPlusNewNames = Prelude.foldl f oldNames mds
      newNames = (\\) oldPlusNewNames oldNames
  return newNames

 where f :: [Name] -> MessageD -> [Name]
       f ns md = let x = from md
                     y = to md
                     res = case x `elem` ns of
                       True -> ns
                       False -> ns ++ [x]
                     res' = case y `elem` res of
                       True -> res
                       False -> res ++ [y]
                 in res'
-}  

startState :: ProtoState
startState = ProtoState empty empty 0 0 0 [] False



