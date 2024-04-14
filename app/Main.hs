{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS -Wall #-}

module Main where

import Control.Lens (makeLenses, (%~), (&))
import Control.Monad (forM_, guard, unless, when)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Raylib.Core (
  beginDrawing,
  c'loadDroppedFiles,
  c'unloadDroppedFiles,
  clearBackground,
  closeWindow,
  endDrawing,
  initWindow,
  isFileDropped,
  isKeyDown,
  isKeyPressed,
  setExitKey,
  setTargetFPS,
  windowShouldClose,
 )
import Raylib.Core.Text (drawText)
import Raylib.Types (
  Color (Color),
  FilePathList (filePathList'paths),
  KeyboardKey (KeyA, KeyDown, KeyEnter, KeyLeft, KeyNull, KeyRight, KeySpace, KeyUp),
  Music,
 )
import Raylib.Util (WindowResources, raylibApplication)
import Raylib.Util.Colors qualified as Colors
import System.Exit (exitSuccess)

import Audio qualified (scanAudio)
import Control.Monad.Reader (MonadReader (ask), ReaderT (ReaderT, runReaderT), withReaderT)
import Foreign (Storable (peek))
import Raylib.Core.Audio (getMasterVolume, getMusicTimeLength, initAudioDevice, isMusicReady, loadMusicStream, loadSound, playMusicStream, playSound, setMasterVolume, updateMusicStream)

default (Int)

whenIO :: IO Bool -> IO () -> IO ()
whenIO condition action = do condition' <- condition; when condition' action

class Cycle a where
  nextCycle :: a -> a
  previousCycle :: a -> a

--

data AppMode = AppModeOne | AppModeMenu | AppModeMediaControl deriving (Eq)

instance Cycle AppMode where
  nextCycle appMode = case appMode of
    AppModeOne -> AppModeMenu
    AppModeMenu -> AppModeMediaControl
    AppModeMediaControl -> AppModeOne

  previousCycle = nextCycle . nextCycle

data AppMenu = Volume | Brightness | Quit deriving (Show, Eq)
instance Cycle AppMenu where
  nextCycle appMenu = case appMenu of
    Volume -> Brightness
    Brightness -> Quit
    Quit -> Volume

  previousCycle = nextCycle . nextCycle

data AppState = AppState
  { _mode :: AppMode
  , _mediaFiles :: [Music]
  , _menu :: AppMenu
  , _window :: WindowResources
  , _counter :: Int
  }

makeLenses ''AppState

defaultState :: WindowResources -> AppState
defaultState w =
  AppState
    { _mode = AppModeOne
    , _mediaFiles = []
    , _menu = Quit
    , _window = w
    , _counter = 0
    }

initApp :: IO AppState
initApp = do
  w <- initWindow 1200 800 "Pokiclone"
  setTargetFPS 60
  initAudioDevice
  song <- loadMusicStream "/home/dk/Music/17 - Maglietta e Jeans.mp3" w
  playMusicStream song
  setExitKey KeyNull
  return $ defaultState w

type AppStateIO = ReaderT AppState IO AppState

data TextF = TextF
  { txtString :: String -- The text content
  , xAxis :: Int -- Position on the x-axis
  , yAxis :: Int -- Position on the y-axis
  , size :: Int -- Font size
  , color :: Color -- Text color
  }

drawTextF :: TextF -> IO ()
drawTextF textf =
  drawText textf.txtString textf.xAxis textf.yAxis textf.size textf.color

drawTxtFromPrompt :: Color -> [String] -> IO ()
drawTxtFromPrompt color text =
  let n = length text
   in when (n /= 0) $
        [0 .. n - 1] `forM_` \i ->
          drawTextF
            TextF
              { txtString = text !! i
              , xAxis = 50
              , yAxis = 100 + 45 * i
              , size = 30
              , color = color
              }

appModeOne :: AppStateIO
appModeOne = do
  lift $ do
    clearBackground (Color 1 20 40 1)
    drawText "POG" 50 50 40 Colors.rayWhite
  readPrompt
 where
  readPrompt :: AppStateIO
  readPrompt = do
    appState <- ask
    lift $ do
      x <- Audio.scanAudio
      drawTxtFromPrompt
        Colors.rayWhite
        (concat x)
    return appState

appModeMenu :: AppStateIO
appModeMenu = do
  lift $ do
    clearBackground $ Color 1 20 40 1
    drawTxtFromPrompt Colors.rayWhite (show <$> [Quit, Volume, Brightness])

  shouldMoveDown <- lift $ isKeyPressed KeyDown
  shouldMoveUp <- lift $ isKeyPressed KeyUp
  appState <- ask
  let appState'
        | shouldMoveDown = appState & menu %~ nextCycle
        | shouldMoveUp = appState & menu %~ previousCycle
        | otherwise = appState
  shouldLowerVolume <- lift $ (&& (appState'._menu == Volume)) <$> isKeyDown KeyLeft
  shouldIncreaseVolume <- lift $ (&& (appState'._menu == Volume)) <$> isKeyDown KeyRight
  if
    | shouldLowerVolume -> changeVolume (\x -> x - (0.1 * x)) >> return appState'
    | shouldIncreaseVolume -> changeVolume (\x -> x + (0.1 * x)) >> return appState'
    | otherwise -> displayMenu appState'
 where
  changeVolume update = lift $ do
    masterVolume <- getMasterVolume
    let newVolume = update masterVolume
    when (newVolume < 0.999 && newVolume > 0.001) $ setMasterVolume newVolume

  displayMenu appState = do
    let menuPosition = case appState._menu of
          Quit -> 0
          Volume -> 1
          Brightness -> 2
    lift $
      drawTextF
        TextF
          { txtString = show appState._menu
          , xAxis = 50
          , yAxis = 100 + 45 * menuPosition
          , size = 30
          , color = Colors.red
          }
        >> return appState

appModeMediaControl :: AppStateIO
appModeMediaControl = do
  maybeUpdatedState <- runMaybeT $ do
    appState <- lift ask
    lift2 $ do
      clearBackground (Color 10 60 90 10)
      drawText "Media Control Panel" 50 50 40 Colors.rayWhite
      drawText (show $ _mediaFiles appState) 50 (100 + 45) 40 Colors.rayWhite
    fileDropped <- lift2 isFileDropped
    guard fileDropped
    filePtr <- lift2 c'loadDroppedFiles
    fileContent <- lift2 $ peek filePtr
    lift2 $ c'unloadDroppedFiles filePtr
    song <- lift2 $ loadMusicStream (head fileContent.filePathList'paths) appState._window
    lift2 $ do
      playMusicStream song
      print song
      print =<< getMusicTimeLength song
    return $ appState & mediaFiles %~ (++ [song])
  maybe ask return maybeUpdatedState
 where
  lift2 = lift . lift

mainLoop :: AppState -> IO AppState
mainLoop appState = do
  beginDrawing
  unless (null appState._mediaFiles) $ do updateMusicStream $ head appState._mediaFiles
  appState' <- runReaderT loop appState
  endDrawing
  print appState'._counter
  if appState'._counter > 10 ^ 3
    then do
      return $ (appState' & counter %~ const 0) & mediaFiles %~ pop
    else return $ appState' & counter %~ (+ 1)
 where
  pop x = case x of _ : xs -> xs; [] -> []
  loop :: AppStateIO = do
    appState' <- switchMode
    withReaderT (const appState') $ case appState._mode of
      AppModeOne -> appModeOne
      AppModeMenu -> appModeMenu
      AppModeMediaControl -> appModeMediaControl
  switchMode :: AppStateIO
  switchMode = do
    shouldGoNext <- lift $ isKeyPressed KeySpace
    if shouldGoNext
      then return $ appState & mode %~ nextCycle
      else return appState

shouldClose :: AppState -> IO Bool
shouldClose appState = do
  pressedEnter <- isKeyPressed KeyEnter
  closeEarly <- windowShouldClose
  return $ (appState._menu == Quit && pressedEnter) || closeEarly

teardown :: AppState -> IO ()
teardown appState = closeWindow appState._window

$(raylibApplication 'initApp 'mainLoop 'shouldClose 'teardown)

{--
 - Create a function to display a list on screen
 - Create a function to play music consecutively
 --}
