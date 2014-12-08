module ProtoState where

import Data.Map hiding ((\\))
import Data.List
import qualified Control.Monad.Trans.State as T
import Control.Monad.Trans(liftIO)

import ParseFile


type Id = Int --Principal Id and Slot Id the same for now
type Name = String

type Message = String
type To = Id --Destination
data Contents = Send Message To
              -- | Blank  --Action-send to self(vertical action bar with text beside?)--
                deriving (Show, Eq)

type Index = Int --Represents a row in the diagram
type Slot = (Index, Contents)
type Outbox = [Slot]
data Principal = Principal
                 { name     :: Name
                 , outbox  :: Outbox
                 } deriving (Eq, Show)


-- TODO: add messageIndex :: Map MessageId Slot
--       AND change Outbox to:  type Outbox = [MessageId] where MessageId = Int
data ProtoState = ProtoState
             { principals        :: Map Id Principal
             , maxPrincipalId    :: Id 
             , maxMessageId      :: Id
             , editMode          :: Bool
             } deriving (Eq, Show)


type Proto = T.StateT ProtoState IO

stateMain = exec pTest

exec :: Proto () -> IO ProtoState
exec p = T.execStateT p startState

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
  

startState :: ProtoState
startState = ProtoState empty 0 0 False



