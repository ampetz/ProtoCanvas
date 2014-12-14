{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Prelude hiding (lookup)
import Graphics.Blank
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Data.Text hiding (map,maximum,minimum,length, find)
import Control.Concurrent.STM
import Control.Concurrent
import Data.Map hiding (map)
import Data.String
import Data.Char
--import System.IO
--import System.Directory
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Binary hiding (get, put)
import Data.List(sortBy, find)

import ProtoState
import ParseFile
import qualified Adul as A
import qualified ISpi as I
import qualified AdulBuffer as AB

type ProtoViewer = ReaderT ViewerParams Canvas
data ViewerParams = ViewerParams
                    { context       :: DeviceContext
                    , state  :: ProtoState
                    , hSpace :: Double
                    , vSpace :: Double  
                    } deriving ()






prompt :: String -> IO FilePath
prompt s = do
  putStrLn s
  command <- getLine
  return command

commandPrompt = Main.prompt "Enter command: "
--savePrompt = prompt "Enter name of file to save to: "


main = do
  state_var <- atomically $ newTVar startState
  let kp = pack "keypress"
      kd = pack "keydown"
  blankCanvas 3000 {events = [kd] } $ \ context -> do
      viewerThread <- forkIO $ viewer context state_var
      execStateT (pcmain context state_var) startState
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




--TODO make list of keywords:  ["save", "add",...] and make sure the user doesn't create a file of the same name as one of them.  OR do commands:  add filename, save filename, etc.
pcmain :: DeviceContext -> TVar ProtoState -> Proto ()
pcmain context state_var = do
  cmd' <- liftIO $ commandPrompt
  let cmd = Prelude.takeWhile (not . isSpace) cmd'
  case cmd of
    "" -> do pcmain context state_var
    "add" -> do ProtoState{..} <- get
                liftIO $ putStrLn "Enter Messge to Add: "
                m <- liftIO getLine
                let md = parseLine m
                addMessageAt (maxRow + 1) md
                pcmain context state_var


    "spi" -> do
     -- runForOutput :: PiProcess ->IO (Either String Result )

      --let eitherResult :: Either String I.Result
      liftIO $ putStrLn $ "\nProtocol: " ++ (show A.inst_m2_shared) ++ "\n"
      eitherResult <- liftIO $ I.runForOutput A.inst_m2_shared
      case eitherResult of
        Prelude.Left s -> liftIO $ putStrLn s
        Prelude.Right (I.Result f g p) -> do
          let mList' = map (AB.xx g) AB.mList
              mds' = map AB.convertMessage mList'
              mds = AB.sortPairs mds'
          --liftIO $ putStrLn $ (show mds)
          liftIO $ putStrLn $ "\n" ++ "Final result: " ++ (show f) ++ "\n"
      
          mapM_ AB.addMessageAt' mds
          pcmain context state_var 


      
    "d" -> do state <- get
              liftIO $ print state
              liftIO $ atomically $
                writeTVar state_var state
              --customDraw context
              pcmain context state_var
    "q" -> do s <- liftIO $ commandPrompt
              liftIO $ putStrLn s
              pcmain context state_var
              return ()
    "a" ->  do
      addFromFile "testFile.txt" --fileName
      pcmain context state_var
    "sa" -> do --state@ProtoState{..} <- get
               fn <- liftIO $ savePrompt
               saveStateAs fn
               --put ProtoState{savedAs = fn, editMode = False, ..}
               --put ProtoState{editMode = False}
               pcmain context state_var
    "s" -> do state@ProtoState{..} <- get
              --let fn = savedAs state
              case (savedAs) of
                "" -> do
                  fn <- liftIO $ savePrompt
                  saveStateAs fn
                  --put startState
                  --put ProtoState{savedAs = sa, ..}
                _ -> do
                  liftIO $ do
                    BS.writeFile savedAs (BS.concat $ LBS.toChunks
                                          (encode state))
                    
                    putStrLn $ "Saved: " ++ savedAs
                  put ProtoState{editMode = False, ..}
              pcmain context state_var
                
    "l" -> do newState <- liftIO loadState
              put newState
              pcmain context state_var
    "e" -> do state@ProtoState{..} <- get
              case editMode of
                True -> do
                  liftIO $ putStrLn "Turning editMode OFF"
                  put ProtoState{editMode = not editMode, ..}
                False -> do
                  liftIO $ putStrLn "Turning editMode ON"
                  put ProtoState{editMode = not editMode, ..}
              pcmain context state_var

    "rm" -> do
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

      pcmain context state_var

    "rp" -> do
      state@ProtoState{..} <- get
      liftIO $ putStrLn "Enter principal number to remove: "
      num <- liftIO readInt
             
      case or [(num > maxCol), (num < 1)] of
        True -> liftIO $ putStrLn "invalid pid"
        False -> do
          removePrincipalAt num
          liftIO $ putStrLn $ "Removed principal" ++ (show num)

      pcmain context state_var

    "c" -> do
      liftIO $ putStrLn "Are you sure you want to clear?(y/n): "
      yn <- liftIO getChar
      case yn of
        'y' -> do
          put startState
          liftIO $ putStrLn "Cleared"
        'n' -> return ()


      pcmain context state_var

    "m" -> do
      liftIO $ flush context
      mEventLoop context state_var
      pcmain context state_var

    "p" -> do
      liftIO $ flush context
      pEventLoop context state_var
      pcmain context state_var


    "rmi" -> do
      liftIO $ putStrLn "Enter message number to reset: " 
      mid <- liftIO $ readInt
      ProtoState {..} <- get
      put ProtoState {mSliderIndividuals = insert mid 0 mSliderIndividuals, ..}
      pcmain context state_var

    "rpi" -> do
      liftIO $ putStrLn "Enter principal number to reset: " 
      pid <- liftIO $ readInt
      ps <- getPrincipals
      let list = toList ps
          maybeP = find (remPred pid)  list
          name = case maybeP of
            Nothing -> "aa"
            Just (n, _) -> n 

      ProtoState {..} <- get
      put ProtoState {pSliderIndividuals = insert name 0 pSliderIndividuals, ..}
      pcmain context state_var
     where
        remPred :: Pos -> (Name,Principal) -> Bool
        remPred cIn (_, Principal col' _) = cIn == col'
       
    "mi" -> do
      liftIO $ flush context
      liftIO $ putStrLn "Enter message number: " 
      mid <- liftIO $ readInt
      mEventLoop' context state_var mid
      pcmain context state_var

    
    "pi" -> do
      liftIO $ flush context
      liftIO $ putStrLn "Enter principal number: " 
      pid <- liftIO $ readInt
      ps <- getPrincipals
      let list = toList ps
          maybeP = find (remPred pid)  list
          name = case maybeP of
            Nothing -> "aa"
            Just (n, _) -> n 

      pEventLoop' context state_var name
      pcmain context state_var

     where
       remPred :: Pos -> (Name,Principal) -> Bool
       remPred cIn (_, Principal col' _) = cIn == col'
      
      
      
    _ -> do liftIO $ putStrLn "Unknown command.  Please retry: " 
            pcmain context state_var


mEventLoop :: DeviceContext -> TVar ProtoState -> Proto ()
mEventLoop context state_var = do
  event <- liftIO $ wait context
  liftIO $ print event
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
  liftIO $ print event
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
  liftIO $ print event
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
  liftIO $ print event
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
       drawDashes (MoveTo start) end (vLineSize) (pack "black")

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
                 Main.Right -> 0
                 Main.Left -> hSpace*2
               hCenter = startX + (hSpace) - adjust
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
               yPos = startY --startY + (vSpace / 2)


           
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
    
    let dashPartition = 100
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

    
    
    
  
  
