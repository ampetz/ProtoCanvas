module ProtoState where

import Data.Map hiding ((\\), null, map)
import Data.List hiding (insert)
import {-qualified-} Control.Monad.Trans.State {-as T-}
import Control.Monad.Trans(liftIO)

import ParseFile


--type Id = Int --Principal Id and Slot Id the same for now
type Name = String
type MessageId = Int

type MessageText = String
type To = Name --Destination
data Contents = Send MessageText To
              -- | Blank  --Action-send to self(vertical action bar with text beside?)--
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


addToPMap :: (Name, Principal) -> Proto ()
addToPMap (n, p) = do
  (ProtoState ps a b c d e f) <- get
  let newPs = insert n p ps
  put (ProtoState newPs a b c d e f)
  

getPrincipals :: Proto (Map Name Principal)
getPrincipals = do
  state <- get
  return (principals state)

doesPrincipalExist :: Name -> Proto Bool
doesPrincipalExist name = do
  ps <- getPrincipals
  return (member name ps)
  
addPrincipalsAt :: Pos -> [Name] -> Proto ()
addPrincipalsAt posIn ns@(name:rest)
  | null ns = return ()
  | otherwise = do
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

removePrincipalAt :: Pos -> Proto ()
removePrincipalAt posIn = do
  upDownCols False posIn



updatePMap :: Map Name Principal -> Proto ()
updatePMap newMap = do
   (ProtoState _ a b c d e f) <- get
   put (ProtoState newMap a b c d e f)


upDownCols :: Bool -> Pos -> Proto ()
upDownCols b posInserted = do
  ps <- getPrincipals
  let list :: [(Name, Principal)]
      list = toList ps
      updatedList = changeCols b posInserted list
  updatePMap (fromList updatedList)
  return ()
{-
upCols :: Pos -> Proto ()
upCols posInserted = do
  ps <- getPrincipals
  let list :: [(Name, Principal)]
      list = toList ps
      updatedList = incrCols posInserted list
  updatePMap (fromList updatedList)
  return ()
-}

changeCols :: Bool -> Pos -> [(Name, Principal)] -> [(Name, Principal)] 
changeCols b pos xs = map (change b pos) xs

{-
incrCols :: Pos -> [(Name, Principal)] -> [(Name, Principal)]
incrCols pos xs = map (change True pos) xs

decrCols :: Pos -> [(Name, Principal)] -> [(Name, Principal)]
decrCols pos xs = map (change False pos) xs
-}


change :: Bool -> Pos -> (Name, Principal) -> (Name, Principal)
change sign pos (name, pIn@(Principal col outbox)) =
  let plusMinus = case sign of
        True -> 1
        False -> (-1)
      newP = case col >= pos of
        True -> Principal (col + plusMinus) outbox
        False -> pIn in
  (name, newP)
  

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



