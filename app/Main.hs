{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS -Wall #-}

module Main where

import Control.Lens (makeLenses, (%~), (&))
import Control.Monad (forM, forM_, guard, unless, when)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.Semigroup
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
  loadDroppedFiles,
  setExitKey,
  setTargetFPS,
  windowShouldClose,
 )
import Raylib.Core.Text (drawText)
import Raylib.Types (
  Color (Color),
  FilePathList (FilePathList, filePathList'paths),
  KeyboardKey (KeyDown, KeyEnter, KeyGrave, KeyLeft, KeyNull, KeyRight, KeySpace, KeyUp),
  Sound (sound'frameCount),
 )
import Raylib.Util (WindowResources, raylibApplication)
import Raylib.Util.Colors qualified as Colors

import Audio qualified (scanAudio)
import Control.Exception (try)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT), withReaderT)
import Data.List (isPrefixOf, isSuffixOf, partition, tails)
import Foreign (Storable (peek))
import Raylib.Core.Audio (getMasterVolume, initAudioDevice, loadMusicStream, loadSound, pauseSound, playMusicStream, playSound, resumeSound, setMasterVolume, stopSound)
import System.Exit (exitWith, exitFailure)

default (Int)

whenIO :: IO Bool -> IO () -> IO ()
whenIO condition action = do condition' <- condition; when condition' action

class Cycle a where
  nextCycle :: a -> a
  previousCycle :: a -> a

--

data AppMode = AppModeOne | AppModeMenu | AppModeMediaControl deriving (Eq)

data Song = Song
  { media :: Sound
  , title :: String
  , duration :: Integer
  }
  deriving (Show)

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
  , _mediaFiles :: [Song]
  , _menu :: AppMenu
  , _window :: WindowResources
  , _counter :: Integer
  , _shouldPlay :: Bool
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
    , _shouldPlay = True
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
    | shouldLowerVolume -> changeVolume $ \x -> x - 0.1 * x
    | shouldIncreaseVolume -> changeVolume $ \x -> x + 0.1 * x
    | otherwise -> displayMenu appState'
  return appState'
 where
  changeVolume update = lift $ do
    masterVolume <- getMasterVolume
    let newVolume = update masterVolume
    when (newVolume < 0.999 && newVolume > 0.001) $ setMasterVolume newVolume

  displayMenu appState =
    let menuPosition = case appState._menu of
          Quit -> 0
          Volume -> 1
          Brightness -> 2
     in lift $
          drawTextF
            TextF
              { txtString = show appState._menu
              , xAxis = 50
              , yAxis = 100 + 45 * menuPosition
              , size = 30
              , color = Colors.red
              }

appModeMediaControl :: AppStateIO
appModeMediaControl = do
  appState <- runMaybeT $ do
    appState <- lift ask
    lift2 $ do
      clearBackground (Color 10 60 90 10)
      drawText "Media Control Panel" 50 50 40 Colors.rayWhite
      drawTxtFromPrompt Colors.rayWhite $ title <$> appState._mediaFiles
    fileDropped <- lift2 isFileDropped
    guard fileDropped
    filePtrs <- lift2 loadDroppedFiles
    files <- case checkIfSupportedFileType filePtrs of
      Left e -> lift2 $ putStrLn e >> return (FilePathList 0 [])
      Right files' -> lift2 $ return files'
    songs <-
      files.filePathList'paths
        `forM` \song ->
          lift2 $ loadSound song appState._window
    let songConfigs =
            (songs `zip` [0 ..])
              `for` \(song, i) ->
                Song
                  { media = song
                  , duration = song.sound'frameCount
                  , title = formatTitle $ files.filePathList'paths !! i
                  }
    lift2 $ do
      putStrLn "INFO: Loading files:" >> print `mapM_` (title <$> songConfigs)
      return $ appState & mediaFiles %~ (++ songConfigs)
  maybe ask return appState
 where
  formatTitle title = case dropWhile (not . isPrefixOf "- ") (tails title) of
    [] -> "" -- If "- " is not found, return an empty string
    (x : _) -> drop 2 x -- Otherwise, return the first occurrence of "- "
  checkIfSupportedFileType :: FilePathList -> Either String FilePathList
  checkIfSupportedFileType filePathList = do
    let fileNames = filePathList.filePathList'paths
        (flacFiles, _) = partition (\path -> ".flac" `isSuffixOf` path) fileNames
    if
      | not (null flacFiles) -> Left "ERROR: Flac files not supported"
      | all (".mp3" `isSuffixOf`) fileNames -> return filePathList
      | otherwise -> Left "ERROR: File type not supported"

lift2 = lift . lift
for = flip map

mainLoop :: AppState -> IO AppState
mainLoop appState = do
  beginDrawing
  appState' <- runReaderT loop appState
  appState'' <- runReaderT appControlAudio appState'
  endDrawing
  runReaderT playMusic appState''
 where
  playMusic :: AppStateIO
  playMusic = do
    appState <- runMaybeT $ do
      appState <- lift ask
      guard (not . null $ appState._mediaFiles)
      lift2 $
        if
          | not appState._shouldPlay -> return appState
          | appState._counter > (head appState._mediaFiles).duration `div` 800 -> do
              return $ (appState & counter %~ const 0) & mediaFiles %~ pop
          | appState._counter == 0 -> do
              playSound (head appState._mediaFiles).media
              return $ appState & counter %~ (+ 1)
          | otherwise -> return $ appState & counter %~ (+ 1)
    maybe ask return appState

  loop :: AppStateIO = do
    appState' <- switchMode
    withReaderT (const appState') $ case appState._mode of
      AppModeOne -> appModeOne
      AppModeMenu -> appModeMenu
      AppModeMediaControl -> appModeMediaControl

  appControlAudio :: AppStateIO = do
    shouldCycleAudio <- lift $ isKeyPressed KeySpace
    appState <- ask
    let currentMediaFiles = appState._mediaFiles
    if
      | shouldCycleAudio && appState._shouldPlay && not (null currentMediaFiles) -> do
          lift $ pauseSound (head appState._mediaFiles).media
          return $ appState & shouldPlay %~ not
      | shouldCycleAudio && not appState._shouldPlay && not (null currentMediaFiles) -> do
          lift $ resumeSound (head appState._mediaFiles).media
          return $ appState & shouldPlay %~ not
      | shouldCycleAudio -> return $ appState & shouldPlay %~ not
      | otherwise -> return appState

  switchMode :: AppStateIO
  switchMode = do
    shouldGoNext <- lift $ isKeyPressed KeyGrave
    if shouldGoNext
      then return $ appState & mode %~ nextCycle
      else return appState

  pop = \case _ : xs -> xs; [] -> []

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
