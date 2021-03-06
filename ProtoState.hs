{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module ProtoState where

import Prelude hiding (lookup)
import Data.Map hiding ((\\), null, foldl,filter)
import qualified Data.Map as M
import Data.List hiding (insert, lookup)
import qualified Data.List as L
import System.Directory
import System.FilePath
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Binary as B
import Data.Word
import Control.Exception

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

instance B.Binary Contents where
  put (Send m t) = do
    B.put (0::Word8)
    B.put m
    B.put t
  put (Action mt) = do
    B.put (1::Word8)
    B.put mt
  get = do x <- B.get :: B.Get Word8
           case x of
             0 -> do m <- B.get
                     t <- B.get
                     return $ Send m t
             1 -> do mt <- B.get
                     return $ Action mt
    

type Pos = Int --Represents a row(for messages) or column(for principals) position in the diagram
data Message = Message { row       :: Pos   -- Pos represents message row
                       , contents  :: Contents
                       } deriving (Eq,Show)
               
type Outbox = [MessageId]
data Principal = Principal
                 { col     :: Pos   --Pos represents principal column
                 , outbox  :: Outbox
                 } deriving (Eq, Show)

instance B.Binary Message where
  put (Message a b) = do
    B.put a
    B.put b
  get = do
    a <- B.get
    b <- B.get
    return $ Message a b

instance B.Binary Principal where
  put (Principal a b) = do
    B.put a
    B.put b
  get = do
    a <- B.get
    b <- B.get
    return $ Principal a b

-- TODO: add messageIndex :: Map MessageId Slot
--       AND change Outbox to:  type Outbox = [MessageId] where MessageId = Int

changeName :: Name -> Name -> Proto ()
changeName nameIn newName = do
  ProtoState{..} <- get
  let maybeP = lookup nameIn pMap
  case maybeP of
    Nothing -> return ()
    Just (Principal col ob) -> do
      let newPmap' = M.delete nameIn pMap
          newPmap = M.insert newName (Principal col ob) newPmap'
      setPMap newPmap
  ms <- getMessages
  let mList = toList ms
  newMlist <- mapM (changePMessages nameIn newName) mList
  let newMmap = fromList newMlist
  setMMap newMmap

changePMessages :: Name -> Name -> (MessageId,Message) -> Proto (MessageId,Message)
changePMessages nameIn newName (mid, m) = do
  ms <- getMessages
  case M.lookup mid ms of
    Nothing -> return (0,Message 0 (Action ""))
    Just (Message row contents) -> do
      case contents of
        Action _ -> return (0, Message 0 (Action ""))
        Send mt t -> do
          let newNameX = case t == nameIn of
                True -> newName
                False -> t
          return (mid, Message row (Send mt newNameX))


setMSlider :: Double -> Proto ()
setMSlider sizeIn = do
  ProtoState{..} <- get
  put ProtoState{mSlider = sizeIn, ..}

setPSlider :: Double -> Proto ()
setPSlider sizeIn = do
  ProtoState{..} <- get
  put ProtoState{pSlider = sizeIn, ..}

setMSlider' :: MessageId -> Double -> Proto ()
setMSlider' i sizeIn = do
  ProtoState{..} <- get
  put ProtoState{mSliderIndividuals = insert i sizeIn mSliderIndividuals, ..}

setPSlider' :: Name -> Double -> Proto ()
setPSlider' n sizeIn = do
  ProtoState{..} <- get
  put ProtoState{pSliderIndividuals = insert n sizeIn pSliderIndividuals, ..}

data ProtoState = ProtoState
             { pMap        :: Map Name Principal
             , mMap          :: Map MessageId Message 
             , maxCol            :: Pos
             , maxRow            :: Pos  
             , mmid              :: MessageId
             , fmids          :: [MessageId]
             , editMode          :: Bool
             , savedAs           :: FilePath
             , mSlider    :: Double
             , pSlider    :: Double
             , mSliderIndividuals :: Map MessageId Double
             , pSliderIndividuals :: Map Name Double
             --, undoList  :: [ProtoState]
             --, redoList  :: [ProtoState]
             } deriving (Eq, Show)

instance B.Binary ProtoState where
  put (ProtoState a b c d e f g h i j k l) = do
    B.put a
    B.put b
    B.put c
    B.put d
    B.put e
    B.put f
    B.put g
    B.put h
    B.put i
    B.put j
    B.put k
    B.put l
    --B.put m
   -- B.put n
  get = do
    a <- B.get
    b <- B.get
    c <- B.get
    d <- B.get
    e <- B.get
    f <- B.get
    g <- B.get
    h <- B.get
    i <- B.get
    j <- B.get
    k <- B.get
    l <- B.get
    --m <- B.get
    --n <- B.get
    return $ ProtoState a b c d e f g h i j k l

startState' :: ProtoState
startState' = ProtoState empty empty 0 0 0 [] False "" 0 0 empty empty --[] []

startState :: ProtoState
startState = ProtoState empty empty 0 0 0 [] False "" 0 0 empty empty --[] []

type Proto = StateT ProtoState IO
data MaybeAfter = End
                  | After Pos
prompt :: String -> IO FilePath
prompt s = do
  putStrLn s --"Enter command:"
  fileName <- getLine
  return fileName --"testFile.txt" --fileName

--commandPrompt = ProtoState.prompt "Enter command: "
savePrompt = ProtoState.prompt "Enter name of file to save to: "

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
  outputState
  incrMaxMid
  {-mds <- liftIO $ fromFile "testFile.txt"
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
  removePrincipalCalled "3" -} -}
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
  ProtoState{..} <- get
  newMid <- getNewMid
  upDownRows True posIn  
  addToMMap (newMid, Message posIn (Send m t))    --Self action logic HERE!!!
  incrMaxRow
  --ps <- getPrincipals
  let maybeP = M.lookup fr pMap
      maybeP' = M.lookup t pMap

  case maybeP of
    Nothing -> do
      addPrincipalAt (maxCol+1) fr
      let newP = Principal (maxCol + 1) [newMid]
          newPMap = M.insert fr newP pMap
      setPMap newPMap
    Just (Principal col outbox) -> do
      let newP = Principal col (outbox ++ [newMid])
          newPMap = M.insert fr newP pMap
      setPMap newPMap

  case maybeP' of
        Nothing -> do
          ProtoState{..} <- get
          addPrincipalAt (maxCol+1) fr
          let newP = Principal (maxCol + 1) []
              newPMap = M.insert t newP pMap
          setPMap newPMap
        Just _ {-(Principal col outbox)-} -> do return ()
          {-let newP = Principal col (outbox ++ [newMid])
              newPMap = M.insert t newP pMap
          setPMap newPMap -}
  

  

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
  ProtoState{..} <- get
  put ProtoState{pSliderIndividuals = insert name 0 pSliderIndividuals, ..}

removePrincipalCalled :: Name -> Proto ()
removePrincipalCalled name = do
  ps <- getPrincipals
  case lookup name ps of
    Nothing -> return ()
    Just (Principal col outbox) -> do
      mapM_ removeMessage outbox  --update(messages that send to principal being removed) OR remove(messages 
      let newPMap = M.delete name ps
      setPMap newPMap
      upDownCols False col
      decrMaxCol

      --removePrincipalAt col


removeMessageAt :: Pos -> Proto ()
removeMessageAt posIn = do
  ms <- getMessages
  let list = toList ms
      maybeM = find (findPred posIn) list
  case maybeM of
    Nothing -> return ()
    Just (mid, _) -> removeMessage mid

   where findPred :: Pos -> (MessageId, Message) -> Bool
         findPred rIn (_, Message r _) = rIn == r
  

removePrincipalAt :: Pos -> Proto ()
removePrincipalAt posIn = do
  ps <- getPrincipals
  let list = toList ps
      maybeP = find (remPred posIn)  list
  case maybeP of
    Nothing -> return ()
    Just (name, _) -> removePrincipalCalled name
 where
   remPred :: Pos -> (Name,Principal) -> Bool
   remPred cIn (_, Principal col' _) = cIn == col'


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
  return $ mmid state


setPMap :: Map Name Principal -> Proto ()
setPMap newMap = do
   ProtoState {..} <- get 
   put ProtoState {pMap = newMap, ..}

setMMap :: Map MessageId Message -> Proto ()
setMMap newMap = do
  ProtoState {..} <- get 
  put ProtoState {mMap = newMap, ..}


addToPMap :: (Name, Principal) -> Proto ()
addToPMap (n, p) = do
  ProtoState {..} <- get 
  put ProtoState {pMap = insert n p pMap, ..}

--addToPMap :: (Name, Principal) -> Proto ()
addToMMap (mid, m) = do
  ProtoState {..} <- get 
  put ProtoState {mMap = insert mid m mMap, ..}
  
getPrincipals :: Proto (Map Name Principal)
getPrincipals = do
  state <- get
  return (pMap state)

getMessages :: Proto (Map MessageId Message)
getMessages = do
  state <- get
  return (mMap state)

getFreeMids :: Proto [MessageId]
getFreeMids = do
  state <- get
  return (fmids state)

pushFreeMid :: MessageId -> Proto ()
pushFreeMid mid = do
  ProtoState {..} <- get 
  put ProtoState {fmids = (fmids ++ [mid]), ..}

popFreeMid :: Proto (Maybe MessageId)
popFreeMid = do
  ProtoState{..} <- get
  case null fmids of
    True -> return Nothing
    False -> do
      put ProtoState {fmids = tail fmids, ..}
      return $ Just (head fmids)

removeFreeMid :: MessageId -> Proto ()
removeFreeMid mid = do
  ProtoState {..} <- get 
  put ProtoState {fmids = L.delete mid fmids, ..}

incrMaxCol :: Proto ()
incrMaxCol = do
  ProtoState {..} <- get 
  put ProtoState {maxCol = (maxCol + 1), ..}

decrMaxCol :: Proto ()
decrMaxCol = do
  ProtoState {..} <- get 
  put ProtoState {maxCol = (maxCol - 1), ..}

incrMaxRow :: Proto ()
incrMaxRow = do
  ProtoState {..} <- get 
  put ProtoState {maxRow = (maxRow + 1), ..}
  
decrMaxRow :: Proto ()
decrMaxRow = do
  ProtoState {..} <- get 
  put ProtoState {maxRow = (maxRow - 1), ..}

incrMaxMid :: Proto ()
incrMaxMid = do
  ProtoState {..} <- get
  put ProtoState {mmid = (mmid + 1), ..}




{-
stateMain = exec pTest

exec :: Proto () -> IO ProtoState
exec p = T.execStateT p startState
-}




addFromFile :: FilePath -> Proto ()
addFromFile fileName = do
  mds <- liftIO $ fromFile fileName
  addMessages mds


addMessages :: [MessageD] -> Proto ()
addMessages mds = do
  ProtoState {..} <- get --_ _ maxCol maxRow _ _ _) <- get
  buildPrincipals mds (maxCol + 1)
  addMessagesAt (maxRow + 1) mds
  return ()


saveStateAs :: String -> Proto ()
saveStateAs fn = do
  --state@ProtoState{..} <- get
  hd <- liftIO $ getHomeDirectory
  liftIO $ createDirectoryIfMissing True (hd </> ".proto")
  --fn <- savePrompt
  let file = hd </> ".proto" </> fn
  exists <- liftIO $ doesFileExist file
  case exists of
    True -> do
      liftIO $ putStrLn "This file already exists.  Do you wish to overwrite?(y,n)"
      yn <- liftIO $ getChar
      case yn of
        'y' -> do
          ProtoState{..} <- get
          put ProtoState{savedAs = file, ..}
          state <- get
          liftIO $ BS.writeFile file (BS.concat $ LBS.toChunks (B.encode state))
          liftIO $ putStrLn $ "Saved to: " ++ file
          --return file
        'n' -> return ()
    False -> do
      ProtoState{..} <- get
      let new = ProtoState{savedAs = file,editMode = False,..}
      --state <- get
      liftIO $ BS.writeFile file (BS.concat $ LBS.toChunks (B.encode new))
      liftIO $ putStrLn $ "Saved to: " ++ file
      put ProtoState{savedAs = file,..}

  return ()

displayDirContents :: FilePath -> IO [(Int,FilePath)]
displayDirContents dirPath = do
  contents' <- getDirectoryContents dirPath
  let contents = filter dotPred contents'
      zipList = zip [1..] contents
  displayDirList zipList
  return zipList
  
 where
   dotPred :: FilePath -> Bool
   dotPred fp = case (head fp) of
     '.' -> False
     _ -> True
   displayDirList :: [(Int,FilePath)] -> IO ()
   displayDirList xs = do
     mapM_ display xs
         
   display :: (Int, FilePath) -> IO ()
   display (i,fp) = do
     putStrLn $ (show i) ++ ") " ++ fp 


readInt :: IO Int
readInt = do
  num' <- getLine
  let num :: Int
      num = read num'
  return num
  
loadState :: IO (ProtoState)
loadState = do
  hd <- getHomeDirectory
  createDirectoryIfMissing True (hd </> ".proto")
  let dirPath = hd </> ".proto"
  putStrLn "Enter the number of the protocol file you wish to load: "
  zipList <- displayDirContents dirPath
  num <- readInt
  let maxNum = length $ L.map fst zipList
  case or [(num > maxNum), (num < 1)] of
    True -> do putStrLn "invalid number. try again"
               return startState
    False -> do
      let file' = snd $ zipList !! (num - 1)
          file = dirPath </> file'
      --loadedSt <- BS.readFile file
      --return $ B.decode (LBS.fromChunks [loadedSt])
      loadedSt <- tryJust (\(e :: IOException) ->
                            return $ Just ()) (BS.readFile file)
      case loadedSt of 
        Left _ -> do
          putStrLn $  "\nFile: "++file ++" exists, but may be corrupted." ++
            "  Loading empty state"
          return $ startState
        Right r -> return $ B.decode (LBS.fromChunks [r]) 
    --False -> return Nothing



{-
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





