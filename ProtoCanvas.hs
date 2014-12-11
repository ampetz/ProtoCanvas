{-# LANGUAGE RecordWildCards #-}
module Main where
import Prelude hiding (lookup)
import Graphics.Blank
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.Text hiding (map,maximum,length)
import Control.Concurrent.STM
import Control.Concurrent
import Data.Map hiding (map)
import Data.String
--import System.IO
--import System.Directory
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Binary hiding (get, put)

import ProtoState
import ParseFile

prompt :: String -> IO FilePath
prompt s = do
  putStrLn s --"Enter command:"
  command <- getLine
  return command --"testFile.txt" --fileName

commandPrompt = Main.prompt "Enter command: "
--savePrompt = prompt "Enter name of file to save to: "


main = do
  state_var <- atomically $ newTVar startState
  blankCanvas {-{ events = ["mousedown"] }-} 3000 $ \ context -> do
      viewerThread <- forkIO $ viewer context state_var
      execStateT (pcmain state_var) startState
      --killThread viewerThread
      return ()
  


viewer :: DeviceContext -> TVar ProtoState -> IO () --Reader Canvas ()
viewer context state_var = do
  state <- readTVarIO state_var
  --do Reader Canvas () here. Reader should contain (DeviceContext, hSpace,vSpace,ProtoState)
  -- customDraw :: Reader ( Canvas () )
  -- canvasAction <- customDraw context
  --THEN execute canvasAction inside send(below)
  send context $ do
    let (w,h) = (width context, height context)
    --clearRect (0,0,w,h)

    customDraw context state --numPs
  atomically $ do
    state' <- readTVar state_var
    if state' == state then retry else return ()
  viewer context state_var

--TODO make list of keywords:  ["save", "add",...] and make sure the user doesn't create a file of the same name as one of them.  OR do commands:  add filename, save filename, etc.
pcmain :: TVar ProtoState -> Proto ()
pcmain state_var = do
  fileName <- liftIO $ commandPrompt
  case fileName of
    "add" -> do ProtoState{..} <- get
                liftIO $ putStrLn "Enter Messge to Add: "
                m <- liftIO getLine
                let md = parseLine m
                addMessageAt (maxRow + 1) md
                pcmain state_var


      
    "d" -> do state <- get
              liftIO $ print state
              liftIO $ atomically $
                writeTVar state_var state
              --customDraw context
              pcmain state_var
    "q" -> do s <- liftIO $ commandPrompt
              liftIO $ putStrLn s
              pcmain state_var
              return ()
    "a" ->  do
          addFromFile "testFile.txt" --fileName
          pcmain state_var
    "sa" -> do state@ProtoState{..} <- get
               fn <- liftIO $ saveStateAs state
               case fn of
                 "" -> return ()
                 _ -> put ProtoState{savedAs = fn, ..}
               pcmain state_var
    "s" -> do state@ProtoState{..} <- get
              --let fn = savedAs state
              case (savedAs) of
                "" -> do
                  sa <- liftIO $ saveStateAs state
                  --put startState
                  put ProtoState{savedAs = sa, ..}
                _ -> do
                  liftIO $ do
                    BS.writeFile savedAs (BS.concat $ LBS.toChunks
                                     (encode state))
                    putStrLn $ "Saved: " ++ savedAs
              pcmain state_var
                  
    "l" -> do newState <- liftIO loadState
              put newState
              pcmain state_var
              
      
      
    _ -> do liftIO $ putStrLn "Unknown command.  Please retry: " 
            pcmain state_var
 

customDraw :: DeviceContext -> ProtoState -> Canvas ()
customDraw context state@ProtoState{..}{-state@(ProtoState pMap mMap maxCol maxRow mmid fmids eMode)-} = do
  let xs = keys pMap --[1..maxCol]
      (w,h) = (width context, height context)
  clearRect (0,0,w,h)
  let (rectW, rectH) = scaler w maxCol
      totalHSpace = getTotalHSpace w rectW (fromIntegral maxCol)
      hSpace = totalHSpace / (fromIntegral maxCol + 1)
      totalVSpace = getTotalVSpace h rectH
      vSpace = totalVSpace / (fromIntegral (maxRow + 1))
  mapM_ (f context hSpace vSpace state) xs
      
  --return ()

 where
   f :: DeviceContext -> Double -> Double -> ProtoState -> String -> Canvas ()
   f c hSpace vSpace state@ProtoState{..}{-(ProtoState pMap mMap maxCol maxRow mmid fmids eMode)-} n = do
     let xPMaybe = lookup n pMap
         (x, ob) = case xPMaybe of
           Nothing -> ((-1), [])
           Just (Principal x'' ob'')  -> (x'', ob'')
         --x = col p
         (w, h) = (width c, height c)
         (rectW, rectH) = scaler w maxCol

         extra = (fromIntegral x - 1) * rectW
         (rx,ry) = (hSpace * fromIntegral x + lrborder + extra, tborder)
     fillAndStrokeRect boxFillColor boxStrokeColor boxStrokeSize (rx,ry)
                       (rectW,rectH)

     let xStart = rx + (rectW / 2)
         start = (xStart, ry+rectH)
         end = (xStart, (h - tborder))
     drawLine (MoveTo start) end vLineSize vLineColor


     let val = 1.04
         {-principals = elems ps
         maybePrincipal :: Maybe Principal
         maybePrincipal = Data.Map.lookup x ps -}

         {-(textString, outbox) = (n,ob){-case maybePrincipal of
           Nothing -> ("", [])
           Just (Principal name outbox) -> (name, outbox) -} -}

         
         slist = keys pMap --Prelude.map name principals
         maxStringL = Prelude.maximum (Prelude.map Prelude.length slist)
         dMax :: Double
         dMax = fromIntegral maxStringL
         minStringL = Prelude.minimum (Prelude.map Prelude.length slist)
         dMin :: Double
         dMin = fromIntegral minStringL
         diff = (dMax - dMin)
         diff' = case diff == 0 of True -> dMax
                                   False -> diff

         slider = pTextSizeSliders !! (x-1)
         fontSize = pTextSizeSlider + slider +
                    ((rectH * (diff + (dMax*val))) / (dMax * dMax))
     drawText fontSize pTextColor "center" "middle" {- MiddleBaseline-}
              (pack n{-textString-}) ((rx + (rectW / 2)), (ry + (rectH / 2)))
     --let mailbox = mailbox
     case (Prelude.null ob{-outbox-}) of
       True -> return ()
       False -> mapM_ (g c hSpace vSpace state x) (ob{-outbox-})
     return ()
     
    where g :: DeviceContext -> Double -> Double -> ProtoState ->
               Pos -> MessageId -> Canvas ()
          g c hSpace vSpace ProtoState{..}{-(ProtoState pMap mMap maxCol maxRow mmid fmids eMode)-}
              x y' = do
            let yMaybe = lookup y' mMap
                (y,contents) = case yMaybe of
                  Nothing -> ((-1), (Action ""))
                  Just (Message y'' contents') -> (y'', contents')
                (w,h) = (width c, height c)
                (rectW, rectH) = scaler w maxCol
                extra = (fromIntegral x - 1) * rectW
                start@(startX, startY) =
                  ( hSpace*fromIntegral x + lrborder + (rectW/2) + extra                         ,tborder + rectH + (vSpace*fromIntegral y) )
                {-maybePrincipal = Data.Map.lookup x ps
                principal = case maybePrincipal of
                  Nothing -> Principal "" []
                  Just p -> p
                --mailbox = outbox principal
                --contents = mailbox !! y -}
            case contents of
              Send m toN -> do
                let pMaybe = lookup toN pMap
                    (toId, _) = case pMaybe of
                      Nothing -> (-1, 0)
                      Just (Principal s _) -> (s, 0)
                    hspace = rectW + hSpace
                    delta = fromIntegral (toId - x)
                    end = (startX + hspace*delta, startY)
                    arrowSize = 10 --hardcoded for now...
                    arrowDir = case x < toId of
                      True -> Main.Right
                      False -> Main.Left
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
                    slist = allMessageTexts --keys pMap --Prelude.map name principals
                    maxStringL = maxLen --Prelude.maximum (Prelude.map Prelude.length slist)
                    dMax :: Double
                    dMax = fromIntegral maxStringL
                    minStringL = Prelude.minimum (Prelude.map Prelude.length slist)
                    dMin :: Double
                    dMin = fromIntegral minStringL
                    diff = (dMax - dMin)
                    diff' = case diff == 0 of
                      True -> dMax
                      False -> diff

                    slider = pTextSizeSliders !! (x-1)
                    val = 1.04
                    fontSize = pTextSizeSlider + slider +
                          (((vSpace* (0.8)) * (diff + (dMax*val))) / (dMax * dMax))
                    xPos = hCenter --startX + (hSpace / 2)
                    yPos = startY --startY + (vSpace / 2)

                drawText fontSize mTextColor "center" "bottom"
                         (pack m) (xPos,yPos)  
                return ()
                 where extractText :: Contents -> MessageText
                       extractText contents = case contents of
                         Send mt to -> mt
                         Action mt -> mt
              --_ -> return ()  TODO Self-Send

            
                    
            return ()
            


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

    
    
    
  
  
