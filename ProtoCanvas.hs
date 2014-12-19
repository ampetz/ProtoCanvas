{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Prelude hiding (lookup)
import Graphics.Blank
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Data.Text hiding (map,maximum,minimum,length, find, replicate, head, tail, null)
import Control.Concurrent.STM
import Control.Concurrent
import Data.Map hiding (map, null)
import Data.String
import Data.Char
--import System.IO
--import System.Directory
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Binary hiding (get, put)
import Data.List(sortBy, find)
import System.IO

import ProtoState
import ParseFile
import qualified Adul as A
import qualified ISpi2 as I
import qualified AdulBuffer as AB
import qualified SpiExamples as SE

type ProtoViewer = ReaderT ViewerParams Canvas
data ViewerParams = ViewerParams
                    { context       :: DeviceContext
                    , state  :: ProtoState
                    , hSpace :: Double
                    , vSpace :: Double  
                    } deriving ()


main = do
  state_var <- atomically $ newTVar startState
  let kp = pack "keypress"
      kd = pack "keydown"
  blankCanvas 3000 {events = [kd] } $ \ context -> do
      viewerThread <- forkIO $ viewer context state_var
      execStateT (pcmain context state_var ([], [])) startState
      --killThread viewerThread
      return ()
  

viewer :: DeviceContext -> TVar ProtoState -> IO () --Reader Canvas ()
viewer context state_var = do
  state <- readTVarIO state_var
  send context $ do
    let (w,h) = (width context, height context)
    clearRect (0,0,w,h)
    customDraw context state --numPs
    
  atomically $ do
    state' <- readTVar state_var
    if state' == state then retry else return ()
  viewer context state_var


displayAndLoop' :: DeviceContext -> TVar ProtoState -> Proto ()
displayAndLoop' c s = do
  displayCanvas s
  pcmain c s ([],[])

displayAndLoop :: DeviceContext -> ProtoState -> TVar ProtoState -> Bool
                  -> ([ProtoState],[ProtoState]) -> Proto ()
displayAndLoop c stateBefore s undoRedo l@(undoList, redoList) = do
  displayCanvas s
  state <- get
  let newLists = case stateBefore == state of
        True -> l
        False -> case undoRedo of True -> l
                                  False -> (stateBefore:undoList, redoList)
  pcmain c s newLists



displayCanvas :: TVar ProtoState -> Proto ()
displayCanvas state_var = do
  state <- get
  liftIO $ do
    --print state
    atomically $ writeTVar state_var state

helpCmdDisplay :: IO ()
helpCmdDisplay = do
  let sorted = sortBy cmdSort commands
      maxL = maximum $ map length (map cmd sorted)
  putStrLn "Commands"
  putStrLn "__________________________________________"
  mapM_ (f maxL) sorted
  putStrLn "\n"
  
 where cmdSort :: (Cmd -> Cmd -> Ordering)
       cmdSort (Cmd a _) (Cmd b _) = compare a b

       f :: Int -> Cmd -> IO ()
       f i (Cmd c d) = let extra = (i - length c) in
         putStrLn $ c ++ (replicate (i+extra) ' ') ++ "-" ++ d
  

pcmain :: DeviceContext -> TVar ProtoState -> ([ProtoState],[ProtoState])
          -> Proto ()
pcmain context state_var (undoList, redoList) = do
  liftIO $ hSetBuffering stdout NoBuffering
  cmd <- liftIO $ commandPrompt

  stateBefore <- get
  case cmd of
    x | x == "" -> do pcmain context state_var (undoList, redoList)

    x | x == zoomCmd -> do
      liftIO $ putStrLn "Enter principal numbers(1,2,...): " 
      s <- liftIO $ getLine
      let names' = commaParse s
      names <- mapM colToName names'

      
      
      messgs'' <- mapM getMessagesFor names
      
      
      let messgs' = Prelude.concat messgs''
          result' = sortBy sortX messgs'
          mX = map snd result'
          messgs = filterMs names mX
          
      liftIO $ putStrLn $ show messgs
      addZoomed names messgs

      
    x | x == changePNameCmd -> do
      liftIO $ putStrLn "Enter principal number: " 
      col <- liftIO $ readInt
      name <- colToName col

      liftIO $ putStrLn "Enter new name: "
      newName <- liftIO getLine

      changeName name newName
      

      return ()
    x | x == undoCmd -> do
      case null undoList of
        True -> do
          liftIO $ putStrLn "Nothing to undo"
          return ()
        False -> do
          currentState <- get
          let newState = head undoList
              newUndoList = tail undoList
              newRedoList = currentState : redoList
          put newState
          changeEditMode (editMode currentState)
          displayAndLoop context stateBefore state_var True (newUndoList,newRedoList)

    x | x == redoCmd -> do
      case null redoList of
        True -> do
          liftIO $ putStrLn "Nothing to redo"
          return ()
        False -> do
          currentState <- get
          let newState = head redoList
              newRedoList = tail redoList
              newUndoList = currentState : undoList
          put newState
          changeEditMode (editMode currentState)
          displayAndLoop context stateBefore state_var True (newUndoList,newRedoList)
      

    x | x == helpCmd -> do
        liftIO $ helpCmdDisplay 

      | x == addMCmd -> do
      ProtoState{..} <- get
      liftIO $ putStrLn "Enter Messge to Add: "
      m <- liftIO getLine
      let md = parseLine m
      liftIO $ putStrLn "Enter position(0 for end): "
      i <- liftIO readInt
      case i of
        0 -> addMessageAt (maxRow + 1) md
        n -> addMessageAt (n) md

      | x == spiCmd -> do
     -- runForOutput :: PiProcess ->IO (Either String Result )

      --let eitherResult :: Either String I.Result

      liftIO $ putStrLn "which?:  "
      liftIO $ putStrLn "1) inst_armored  "
      liftIO $ putStrLn "2) inst_m2_shared  "
      i <- liftIO readInt

      
      let abc = case i of
            1 -> SE.inst_armored
            2 -> A.inst_m2_shared
      liftIO $ putStrLn $ "\nProtocol: " ++ (show abc) ++ "\n"
      eitherResult <- liftIO $ I.runForOutput abc
      case eitherResult of
        Prelude.Left s -> liftIO $ putStrLn s
        Prelude.Right (I.Result f g p) -> do
          let mList' = map (AB.xx g) (AB.mList abc)
              mds' = map AB.convertMessage mList'
              mds = AB.sortPairs mds'
          --liftIO $ putStrLn $ (show mds)
          liftIO $ putStrLn $ "\n" ++ "Final result: " ++ (show f) ++ "\n"
      
          mapM_ AB.addMessageAt' mds
          --mapM_ addMessagesAt 0 mds
          
      | x == displayCmd -> do
      ProtoState{..} <- get
      liftIO $ putStrLn "Enter File to add: "
      m <- liftIO getLine
      mds <- liftIO $ fromFile m
      addMessagesAt (maxRow + 1) mds
           
      
      --displayCanvas state_var 
      
      | x == quitCmd -> do
      s <- liftIO $ commandPrompt
      liftIO $ putStrLn s

      | x == loadFileCmd -> do
      liftIO $ putStrLn "Enter File to load: "
      m <- liftIO getLine
      addFromFile m
      
      | x == addFileCmd ->  do
      addFromFile "testFile.txt" --fileName

      | x == saveAsCmd -> do --state@ProtoState{..} <- get
               fn <- liftIO $ savePrompt
               saveStateAs fn
               --put ProtoState{savedAs = fn, editMode = False, ..}
               --put ProtoState{editMode = False}
               --pcmain context state_var

      | x == saveCmd -> do
      ProtoState{..} <- get
              --let fn = savedAs state
      case (savedAs) of
        "" -> do
          fn <- liftIO $ savePrompt
          saveStateAs fn
        _ -> do
          liftIO $ do
            let new = ProtoState{editMode = False,..}
            BS.writeFile savedAs (BS.concat $ LBS.toChunks (encode new))
            putStrLn $ "Saved: " ++ savedAs
          --put ProtoState{editMode = False, ..}

      
      
      | x == loadCmd -> do
      newState <- liftIO loadState
      put newState
      displayAndLoop' context state_var

      | x == editModeCmd -> do 
      state@ProtoState{..} <- get
      let s = case editMode of True -> "OFF"
                               False -> "ON"
      liftIO $ putStrLn $ "Turning editMode " ++ s
      put ProtoState{editMode = not editMode, ..}

      | x == removeMCmd -> do
      state@ProtoState{..} <- get
      liftIO $ putStrLn "Enter message number to remove: "
      num <- liftIO readInt
      liftIO $ putStrLn (show num)
        
      case or [(num > maxRow), (num < 1)] of
        True -> liftIO $ putStrLn "invalid mid"
        False -> do
          removeMessageAt num
          pruneOutboxes
          liftIO $ putStrLn $ "Removed message" ++ (show num)

      | x == removePCmd ->do
      state@ProtoState{..} <- get
      liftIO $ putStrLn "Enter principal number to remove: "
      num <- liftIO readInt
             
      case or [(num > maxCol), (num < 1)] of
        True -> liftIO $ putStrLn "invalid pid"
        False -> do
          removePrincipalAt num
          liftIO $ putStrLn $ "Removed principal" ++ (show num)

      | x == clearCmd -> do
      liftIO $ putStr "Are you sure you want to clear?(y/n): "
      yn <- liftIO getLine
      case (head yn) of
        'y' -> do
          put startState
          liftIO $ putStrLn "Cleared"
        'n' -> return ()
        _ -> do
          liftIO $ putStrLn "Did not enter y or n"
          return ()
      liftIO $ hFlush stdout

      | x == mSliderCmd -> do
      liftIO $ flush context
      mEventLoop context state_var

      | x == pSliderCmd -> do
      liftIO $ flush context
      pEventLoop context state_var

      | x == mIndResetSliderCmd-> do
      liftIO $ putStrLn "Enter message number to reset: " 
      mid <- liftIO $ readInt
      ProtoState {..} <- get
      put ProtoState {mSliderIndividuals = insert mid 0 mSliderIndividuals, ..}

      | x == pIndResetSliderCmd -> do
      liftIO $ putStrLn "Enter principal number to reset: " 
      col <- liftIO $ readInt
      name <- colToName col
      ProtoState {..} <- get
      put ProtoState {pSliderIndividuals = insert name 0 pSliderIndividuals, ..}

      | x == mIndSliderCmd -> do
      liftIO $ flush context
      liftIO $ putStrLn "Enter message number: " 
      mid <- liftIO $ readInt
      mEventLoop' context state_var mid
    
      | x == pIndSliderCmd -> do
      liftIO $ flush context
      liftIO $ putStrLn "Enter principal number: " 
      col <- liftIO $ readInt
      name <- colToName col
      pEventLoop' context state_var name

      | otherwise -> do
      liftIO $ putStrLn "Unknown command. Please retry: " 


  let undoRedoBool = cmd `elem` [undoCmd, redoCmd, saveCmd, saveAsCmd, loadCmd, editModeCmd]
  displayAndLoop context stateBefore state_var undoRedoBool (undoList, redoList)


customDraw :: DeviceContext -> ProtoState -> Canvas ()
customDraw context state@ProtoState{..}= do
    let (w,h) = (width context, height context)
        (rectW, rectH) = scaler w maxCol
        totalHSpace = getTotalHSpace w rectW (fromIntegral maxCol)
        hSpace = totalHSpace / (fromIntegral maxCol + 1)
        totalVSpace = getTotalVSpace h rectH
        vSpace = totalVSpace / (fromIntegral (maxRow + 1))

        vp = ViewerParams context state hSpace vSpace
        principalNames = keys pMap
    runReaderT (mapM_ drawPrincipals principalNames) vp
   

 where
   drawPrincipals :: Name -> ProtoViewer ()
   drawPrincipals n = do
     ViewerParams{..} <- ask
     let xPMaybe = lookup n pMap
         (x, ob) = case xPMaybe of
           Nothing -> ((-1), [])
           Just (Principal x'' ob'')  -> (x'', ob'')
     lift $ do
       let (w, h) = (width context, height context) --x = col p
           (rectW, rectH) = scaler w maxCol

           extra = (fromIntegral x - 1) * rectW
           (rx,ry) = (hSpace * fromIntegral x + lrborder + extra, tborder)
       fillAndStrokeRect boxFillColor boxStrokeColor boxStrokeSize
             (rx,ry) (rectW,rectH)

       let xStart = rx + (rectW / 2)
           start = (xStart, ry+rectH)
           end = (xStart, (h - tborder))
       --drawLine (MoveTo start) end vLineSize vLineColor
       drawDashes (MoveTo start) end (vLineSize - 1) (pack "black")

       let editColCoor = ((fst start) + rectW/6, (snd start) + rectH/1.5)

           val = 1.04
           editColText = case editMode of
             True -> "(" ++ (show x) ++ ")"
             False -> ""
         
           slist = keys pMap --Prelude.map name principals
           maxStringL = Prelude.maximum (Prelude.map Prelude.length slist)
                      + length editColText
           dMax :: Double
           dMax = fromIntegral maxStringL
           minStringL = Prelude.minimum (Prelude.map Prelude.length slist)
           dMin :: Double
           dMin = fromIntegral minStringL
           diff = (dMax - dMin)
           diff' = case diff == 0 of
             True -> dMax
             False -> diff

           slider = pSlider --pTextSizeSliders !! (x-1)
           maybeSliderI = lookup n pSliderIndividuals
           sliderI = case maybeSliderI of
             Nothing -> 0
             Just x -> x
           fontSize = slider + sliderI + 
                    ((rectH * (diff + (dMax*val))) / (dMax * dMax))

         
       drawText fontSize pTextColor "center" "middle" {- MiddleBaseline-}
              (pack (n++editColText){-textString-})
              ((rx + (rectW / 2)), (ry + (rectH / 2)))
     --let mailbox = mailbox

     {-drawText (fontSize / 2) pTextColor "start" "alphabetic"
              (pack $ show x) editColCoor -}
     case (Prelude.null ob{-outbox-}) of
       True -> return ()
       False -> mapM_ (g x) (ob{-outbox-})

    -- return canvasAction


   g :: Pos -> MessageId -> ProtoViewer ()
   g x y' = do
     ViewerParams{..} <- ask
     lift $ do
       let yMaybe = lookup y' mMap
           (y,contents) = case yMaybe of
             Nothing -> ((-1), (Action ""))
             Just (Message y'' contents') -> (y'', contents')
           (w,h) = (width context, height context)
           (rectW, rectH) = scaler w maxCol
           extra = (fromIntegral x - 1) * rectW
           start@(startX, startY) =
             ( hSpace*fromIntegral x + lrborder + (rectW/2) + extra                         ,tborder + rectH + (vSpace*fromIntegral y) )

       case contents of
         Send m toN -> do
           let pMaybe = lookup toN pMap
               (toId, _) = case pMaybe of
                 Nothing -> (-1, 0)
                 Just (Principal s _) -> (s, 0)
               hspace = rectW + hSpace
               delta = fromIntegral (toId - x)
               end = case toId of
                 (-1) -> (startX, startY)
                 _ -> (startX + hspace*delta, startY)
               arrowSize = 10 --hardcoded for now...
               arrowDir = case x < toId of
                 True -> Main.Right
                 False -> Main.Left

           case toId of
             (-1) -> do
               drawLineAndArrow (MoveTo start)
                      ((fst end) - hSpace/2,snd end) hLineSize (pack "blue")
                      arrowSize arrowDir --TODO: fix this

             _ -> do
               drawLineAndArrow (MoveTo start) end hLineSize hLineColor
                                     arrowSize arrowDir
               
           let adjust = case arrowDir of
                 Main.Right -> 1
                 Main.Left -> (-1)
           let hCenter = startX + (adjust * abs(delta) * hSpace)
               messages = elems mMap
               nMes = length messages
               contents''' = map ProtoState.contents messages
               allMessageTexts = map extractText contents'''
               allMtLengths = map length allMessageTexts
               maxLen' = maximum allMtLengths
               maxLen = case maxLen' of
                 1 -> 3
                 2 -> 3
                 _ -> maxLen'
               slist = allMessageTexts 
               maxStringL = maxLen
               dMax :: Double
               dMax = fromIntegral maxStringL
               minStringL = minimum (Prelude.map length slist)
               dMin :: Double
               dMin = fromIntegral minStringL
               diff = (dMax - dMin)
               diff' = case diff == 0 of
                 True -> dMax
                 False -> diff
                   
               --slider = pTextSizeSliders !! (x-1)
               maybeSliderI = lookup y' mSliderIndividuals
               sliderI = case maybeSliderI of
                 Nothing -> 0
                 Just x -> x
               val = 1.04
               fontSize = mSlider + sliderI +
                          (((vSpace* (0.8)) * (diff + (dMax*val))) / (dMax * dMax))
               xPos = hCenter --startX + (hSpace / 2)
               yPos = startY - (vSpace * (0.05)) --startY + (vSpace / 2)


           
           drawText (fontSize + mSlider) mTextColor "center" "bottom"
                         (pack m) (xPos,yPos)

           let editRowCoor = case arrowDir of
                 Main.Right -> ((fst start){--rectW/2-}, (snd start))
                 Main.Left -> ((fst end) {-- rectW/2-}, (snd end))
                   
                      

           case editMode of
             True -> let editRowText = "(" ++ (show y) ++ ")" in
                    drawText fontSize mTextColor "right" "middle"
                             (pack editRowText) editRowCoor
             False -> return ()
           --return ()
      where extractText :: Contents -> MessageText
            extractText contents = case contents of
              Send mt to -> mt
              Action mt -> mt
              --_ -> return ()  TODO Self-Send

            
                    
           -- return ()
            


lrborder :: Double
lrborder = 10

tborder :: Double
tborder = 10

boxFillColor :: Text
boxFillColor = pack "white"

boxStrokeColor :: Text
boxStrokeColor = pack "black"
boxStrokeSize = 4

vLineColor = pack "black"
vLineSize = 2

hLineColor = pack "black"
hLineSize = 1

pTextColor = pack "black"
pTextSizeSlider = --7
                  0

mTextColor = pack "black"

pTextSizeSliders = ---[0,90,90]
                   repeat 0

getTotalHSpace :: Double -> Double -> Double -> Double
getTotalHSpace w rectW numPrincipals =
  w - (lrborder * 2) - (numPrincipals * rectW)

getTotalVSpace :: Double -> Double -> Double
getTotalVSpace h rectH = h - (tborder * 2) - rectH

scaler :: Double -> Int -> (Double, Double)
scaler width t = (rectWS * (1), rectWS / 2)
 where val = 1
       rectWS = val * (width - 2*lrborder) / ((fromIntegral t*val) + fromIntegral t + 1)
         --( 2 * fromIntegral width - 4*lrborder) / (3*fromIntegral t + 1)


  
type Coor = (Double, Double)
type Color = Text

drawText :: Double -> Color -> String{-TextAnchorAlignment-} ->
            String{-TextBaselineAlignment-} -> Text -> Coor -> Canvas ()
drawText fontSize textColor align baseline message (x,y) =
  saveRestore $ do
    let fontString = show fontSize
    font (pack (fontString ++ "pt Courier"))
    fillStyle textColor
    textAlign (fromString align)
    textBaseline (fromString baseline)
    fillText(message, x, y)

type RectDimensions = (Double, Double)

fillAndStrokeRect :: Color -> Color -> Double ->
                     Coor -> RectDimensions -> Canvas ()
fillAndStrokeRect fillColor strokeColor strokeSize (x,y) (rectW, rectH) =
  saveRestore $ do
    fillStyle fillColor
    fillRect(x,y,rectW,rectH)

    lineWidth strokeSize
    strokeStyle strokeColor
    strokeRect(x,y,rectW,rectH)

data Start = MoveTo Coor
            | Stay
type End = Coor

drawLineAndArrow :: Start -> End -> Double -> Color ->
                    Double -> Direction -> Canvas ()
drawLineAndArrow maybeMove (dx,dy) strokeSize strokeColor arrowLen arrowDir =
  saveRestore $ do
    beginPath()
    case maybeMove of
      MoveTo (mx,my) -> moveTo(mx,my)
      Stay -> return ()
    lineTo(dx,dy)
    

    let size = case arrowDir of
          Main.Right -> arrowLen
          Main.Left -> arrowLen * (-1)
    lineTo(dx-size, dy-size)
    moveTo(dx,dy)
    lineTo(dx-size,dy+size)
    
    lineWidth strokeSize
    strokeStyle strokeColor
    stroke()
    closePath()


drawDashes :: Start -> End -> Double -> Color -> Canvas ()
drawDashes maybeMove (dx,dy) strokeSize strokeColor = do
  saveRestore $ do
    beginPath()
    let (mx,my) = case maybeMove of
          MoveTo (mx,my) -> (mx,my)
          Stay -> (-1,-1)
    case maybeMove of
      MoveTo _ -> moveTo (mx,my)
      Stay -> return ()
    
    let dashPartition = 200
        lineLen = dy - my
        frac = lineLen / dashPartition / 6
        incrs = [0..(dashPartition)]
        startYs' = map (*(lineLen / dashPartition)) incrs
        startYs = map (+my) startYs'
        starts = map ((,) mx) startYs

    --drawLenV (strokeSize-1) (pack "black") frac (mx,my)
    mapM_ (drawLenV strokeSize strokeColor frac) starts
    --return ()

--TODO:  f of g in draw
drawLenV :: Double -> Color -> Double -> Coor -> Canvas ()
drawLenV strokeSize strokeColor dY (startX, startY) = do
  drawLine (MoveTo (startX, startY)) (startX, startY + dY) strokeSize
           strokeColor
 

drawLine :: Start -> End -> Double -> Color -> Canvas ()
drawLine maybeMove (dx,dy) strokeSize strokeColor = do
  saveRestore $ do
    beginPath()
    case maybeMove of
      MoveTo (mx,my) -> moveTo(mx,my)
      Stay -> return ()
    lineTo(dx,dy)
    lineWidth strokeSize
    strokeStyle strokeColor
    stroke()
    closePath()

data Direction = Left | Right

filterMs :: [Name] -> [MessageD] -> [MessageD]
filterMs ns mdsIn = Prelude.filter (f ns) mdsIn

 where f :: [Name] -> MessageD -> Bool
       f ns (MessageD fr t m) = and [fr `elem` ns, t `elem` ns]

getMessagesFor :: Name -> Proto [(Pos,MessageD)]
getMessagesFor name = do
  ProtoState{..} <- get

  let maybeP = lookup name pMap
  case maybeP of
    Nothing -> return []
    Just (Principal c ob) -> do
      result' <- mapM (messageUp name) ob
      
      return $ result'

      
changeEditMode :: Bool -> Proto ()
changeEditMode b = do
  ProtoState{..} <- get
  put ProtoState{editMode = b, ..}
  
  
messageUp :: Name -> MessageId -> Proto (Pos, MessageD)
messageUp name mid = do
  let def = (0,MessageD "" "" "")
            
  ms <- getMessages
  let message = case lookup mid ms of
        Nothing -> def
        Just (Message row contents) -> case contents of
          Send mt t -> (row, MessageD name t mt)
          _ -> def
         
  return $ message
         

addZoomed :: [Name] -> [MessageD] -> Proto ()
addZoomed newPNames mds = do
  put startState
  addPrincipalsAt 1 newPNames
  addMessagesAt 1 mds

colToName :: Pos -> Proto Name
colToName pos = do
  ps <- getPrincipals
  let list = toList ps
      maybeP = find (remPred pos)  list
      name = case maybeP of
        Nothing -> ""
        Just (n, _) -> n
  return name
 where
   remPred :: Pos -> (Name,Principal) -> Bool
   remPred cIn (_, Principal col' _) = cIn == col'

mEventLoop :: DeviceContext -> TVar ProtoState -> Proto ()
mEventLoop context state_var = do
  event <- liftIO $ wait context
  --liftIO $ print event
  case eWhich event of
        Nothing -> do liftIO $ putStrLn "DONT THINK I SHOULD GET HERE!!!"
                      mEventLoop context state_var
        Just x -> do
          case x of
            13 -> return ()
            _ -> do
              let adjust = case x of
                    38 -> 1
                    40 -> (-1)
                    _ -> 0
              ProtoState{..} <- get
              setMSlider (mSlider + adjust)
              state <- get
              --liftIO $ print state
              liftIO $ atomically $
                writeTVar state_var state
              mEventLoop context state_var
              
pEventLoop :: DeviceContext -> TVar ProtoState -> Proto ()
pEventLoop context state_var = do
  event <- liftIO $ wait context
  --liftIO $ print event
  case eWhich event of
        Nothing -> do liftIO $ putStrLn "DONT THINK I SHOULD GET HERE!!!"
                      pEventLoop context state_var
        Just x -> do
          case x of
            13 -> return ()
            _ -> do
              let adjust = case x of
                    38 -> 1
                    40 -> (-1)
                    _ -> 0
              ProtoState{..} <- get
              setPSlider (pSlider + adjust)
              state <- get
              --liftIO $ print state
              liftIO $ atomically $
                writeTVar state_var state
              pEventLoop context state_var

mEventLoop' :: DeviceContext -> TVar ProtoState -> MessageId -> Proto ()
mEventLoop' context state_var mid = do
  event <- liftIO $ wait context
  --liftIO $ print event
  case eWhich event of
        Nothing -> do liftIO $ putStrLn "DONT THINK I SHOULD GET HERE!!!"
                      mEventLoop' context state_var mid
        Just x -> do
          case x of
            13 -> return ()
            _ -> do
              let adjust = case x of
                    38 -> 1
                    40 -> (-1)
                    _ -> 0
              ProtoState{..} <- get
              let maybeSliderI = lookup mid  mSliderIndividuals
                  sliderI = case maybeSliderI of
                    Nothing -> 0
                    Just x -> x
              setMSlider' mid (sliderI + adjust)
              state <- get
              --liftIO $ print state
              liftIO $ atomically $
                writeTVar state_var state
              mEventLoop' context state_var mid

pEventLoop' :: DeviceContext -> TVar ProtoState -> Name -> Proto ()
pEventLoop' context state_var n = do
  event <- liftIO $ wait context
  --liftIO $ print event
  case eWhich event of
        Nothing -> do liftIO $ putStrLn "DONT THINK I SHOULD GET HERE!!!"
                      pEventLoop' context state_var n
        Just x -> do
          case x of
            13 -> return ()
            _ -> do
              let adjust = case x of
                    38 -> 1
                    40 -> (-1)
                    _ -> 0
              ProtoState{..} <- get
              let maybeSliderI = lookup n pSliderIndividuals
                  sliderI = case maybeSliderI of
                    Nothing -> 0
                    Just x -> x
              setPSlider' n (sliderI + adjust)
              state <- get
              --liftIO $ print state
              liftIO $ atomically $
                writeTVar state_var state
              pEventLoop' context state_var n

sortX :: (Pos, MessageD) -> (Pos, MessageD) -> Ordering
sortX (x,_) (y,_) = compare x y

linePrompt :: String -> IO FilePath
linePrompt s = do
  putStr s
  --hFlush stdout
  command' <- getLine
  let command = Prelude.takeWhile (not . isSpace) command'
  return command

commandPrompt = linePrompt "Enter command(h for list of commands): "
--savePrompt = prompt "Enter name of file to save to: "

data Cmd = Cmd {cmd :: String, descr :: String}



commands :: [Cmd]
commands = map f xs
 where f :: (String,String) -> Cmd
       f (s,d) = Cmd s d

       xs =
         [
           {-(helpCmd, "Help"),-}
           (addMCmd, "Add a message"),
           (spiCmd, "Run spi"), 
           {-(quitCmd, "Quit"),-}
           (addFileCmd, "Add from testFile.txt"),
           {-(saveCmd, "Save"),
           (saveAsCmd, "Save As"),-} 
           (loadCmd, "Load"), 
           (editModeCmd, "Turn edit mode on/off"), 
           (removeMCmd, "Remove a message"), 
           (removePCmd, "Remove a principal"), 
           (clearCmd, "Clear"), 
           (mSliderCmd, "Overall Message size slider"), 
           (pSliderCmd, "Overall Principal size slider"), 
           (pIndSliderCmd, "Individual Principal size slider"), 
           (mIndSliderCmd, "Individual Message size slider"), 
           (mIndResetSliderCmd, "Reset Individual Message size slider"), 
           (pIndResetSliderCmd, "Individual Principal size slider"),
           (undoCmd, "Undo the previous change"),
           (redoCmd, "Redo the previous undo"),
           (changePNameCmd, "Change p name"),
           (zoomCmd, "zoom to principals"),
           (displayCmd, "Append from file"),
           (loadFileCmd, "Load from a file")
           ]

helpCmd :: String
helpCmd = "h"
addMCmd :: String
addMCmd = "add"
spiCmd = "spi"
displayCmd = "d"
quitCmd = "q"
addFileCmd = "a"
saveCmd = "s"
saveAsCmd = "sa"
loadCmd = "l"
editModeCmd = "e"
removeMCmd = "rm"
removePCmd = "rp"
clearCmd = "c"
mSliderCmd = "m"
pSliderCmd = "p"
pIndSliderCmd = "pi"
mIndSliderCmd = "mi"
mIndResetSliderCmd = "rmi"
pIndResetSliderCmd = "rpi"
undoCmd = "u"
redoCmd = "r"
changePNameCmd = "cn"
zoomCmd = "z"
loadFileCmd = "aa"



{-
drawLineAndArrow :: Start -> End -> Double -> Color ->
                    Double -> Direction -> Canvas ()
drawLineAndArrow maybeMove (dx,dy) strokeSize strokeColor arrowLen dir =
  {-saveRestore $-} do
    --beginPath()
    drawLine maybeMove (dy,dy) strokeSize strokeColor
{-
    strokeStyle strokeColor
    lineTo(dx - arrowLen, dy - arrowLen)
    moveTo(dx,dy)
    lineTo(dx - arrowLen, dy + arrowLen)
    lineWidth strokeSize
    stroke() -}
    --closePath()

-}

    
    
    
  
  
