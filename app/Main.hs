{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS -Wall #-}

module Main where

import Control.Lens (makeLenses, reuse, (%~), (&))
import Control.Monad (forM, forM_, guard, unless, void, when)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import Raylib.Core (
  beginDrawing,
  clearBackground,
  closeWindow,
  endDrawing,
  getScreenHeight,
  getScreenWidth,
  initWindow,
  isFileDropped,
  isKeyDown,
  isKeyPressed,
  loadDroppedFiles,
  setExitKey,
  setTargetFPS,
  windowShouldClose,
 )
import Raylib.Core.Text (c'loadFont, c'loadFontEx, c'unloadFont, drawText, drawTextEx, getFontDefault, loadFont, loadFontEx)
import Raylib.Types (
  Color (Color),
  FilePathList (filePathList'paths),
  Font,
  KeyboardKey (KeyDown, KeyEnter, KeyGrave, KeyLeft, KeyNull, KeyRight, KeySpace, KeyUp),
  Music,
  Vector2 (Vector2),
 )
import Raylib.Util (WindowResources, raylibApplication)
import Raylib.Util.Colors qualified as Colors

import Audio qualified (scanAudio)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT), withReaderT)
import Data.List (isSuffixOf)
import Foreign (Storable (peek), nullPtr)
import Foreign.C (newCString)
import Raylib.Core.Audio (getMasterVolume, getMusicTimeLength, initAudioDevice, loadMusicStream, pauseMusicStream, playMusicStream, resumeMusicStream, seekMusicStream, setMasterVolume, updateMusicStream)

default (Int)

whenIO :: IO Bool -> IO () -> IO ()
whenIO condition action = do condition' <- condition; when condition' action

class Cycle a where
  nextCycle :: a -> a
  previousCycle :: a -> a

--

data AppMode = AppModeOne | AppModeMenu | AppModeMediaControl deriving (Eq)

data Song = Song
  { media :: Music
  , title :: String
  , duration :: Integer
  }
  deriving (Show)

instance Cycle [a] where
  nextCycle [] = []
  nextCycle (x : xs) = xs ++ [x]
  previousCycle [] = []
  previousCycle xs = last xs : init xs

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
  , _font :: [Font]
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
    , _font = []
    }

initApp :: IO AppState
initApp = do
  w <- initWindow 1200 800 "Pokiclone"
  setTargetFPS 60
  fontReg <- loadFont' "./resources/sever-sans-font/SeverSansBook-nRRvP.ttf"
  fontBold <- loadFont "./resources/asimov-font/AsimovWide-0qrG.otf" w
  initAudioDevice
  song <- loadMusicStream "/home/dk/Music/17 - Maglietta e Jeans.mp3" w
  playMusicStream song
  setExitKey KeyNull
  return $ defaultState w & font %~ (++ [fontReg, fontBold])
 where
  loadFont' :: String -> IO Font
  loadFont' txt = do
    txt'c <- newCString txt
    font' <- c'loadFontEx txt'c 32 nullPtr 0
    peek font'

type AppStateIO = ReaderT AppState IO AppState

data TextF = TextF
  { txtString :: String -- The text content
  , xAxis :: Float -- Position on the x-axis
  , yAxis :: Float -- Position on the y-axis
  , size :: Float -- Font size
  , color :: Color -- Text color
  }

drawTextF :: TextF -> AppStateIO
drawTextF textf = do
  -- void DrawTextEx(Font font, const char *text, Vector2 position, float fontSize, float spacing, Color tint); // Draw text using font and additional parameters
  appState <- ask
  screenWidth <- lift getScreenWidth
  screenHeight <- lift getScreenHeight
  lift $
    drawTextEx
      (appState._font !! 0)
      textf.txtString
      ( Vector2
          (textf.xAxis * (0.001 * fromIntegral screenWidth))
          (textf.yAxis + (0.1 * fromIntegral screenHeight))
      )
      (textf.size * 0.2 + 0.01 * fromIntegral (screenWidth + screenHeight))
      2.0
      textf.color
  return appState

drawTxtFromPrompt :: Color -> [String] -> AppStateIO
drawTxtFromPrompt color text = do
  let n = length text
  screenHeight <- lift getScreenHeight
  when (n /= 0) $
    [0 .. n - 1] `forM_` \i ->
      drawTextF
        ( TextF
            { txtString = text !! i
            , xAxis = 50.0
            , yAxis = 100.0 + 45.0 * fromIntegral i * (0.001 * fromIntegral screenHeight)
            , size = 30.0
            , color = color
            }
        )
  ask

appModeOne :: AppStateIO
appModeOne = do
  lift $ clearBackground (Color 1 20 40 1)
  readPrompt
  drawTextF $ TextF "POG" 50 50 120 Colors.rayWhite
 where
  readPrompt :: AppStateIO
  readPrompt = do
    x <- lift Audio.scanAudio
    drawTxtFromPrompt
      Colors.rayWhite
      (concat x)

appModeMenu :: AppStateIO
appModeMenu = do
  lift $ clearBackground $ Color 1 20 40 1
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
    | otherwise -> void displayMenu
  return appState'
 where
  changeVolume update = lift $ do
    masterVolume <- getMasterVolume
    let newVolume = update masterVolume
    when (newVolume < 0.999 && newVolume > 0.001) $ setMasterVolume newVolume

  displayMenu :: AppStateIO
  displayMenu = do
    appState <- ask
    case appState._menu of
      Quit -> drawTxtFromPrompt Colors.red [show Quit, "", ""]
      Volume -> drawTxtFromPrompt Colors.red ["", show Volume, ""]
      Brightness -> drawTxtFromPrompt Colors.red ["", "", show Brightness]

appModeMediaControl :: AppStateIO
appModeMediaControl = do
  appState <- ask
  appState <- runMaybeT $ do
    lift2 $ clearBackground $ Color 1 20 40 1
    lift $ void $ drawTextF $ TextF "Music Queue" 50 10 120 Colors.rayWhite
    let currSongs = take 15 (title <$> appState._mediaFiles)
    lift $ void $ drawTxtFromPrompt (Color 255 0 255 255) currSongs
    (lift . void) $
      drawTxtFromPrompt
        Colors.red
        ( case currSongs of
            [] -> []
            _ -> head currSongs : replicate (length currSongs - 1) ""
        )
    fileDropped <- lift2 isFileDropped
    guard fileDropped
    filePtrs <- lift2 loadDroppedFiles
    let files = filter (\x -> ".mp3" `isSuffixOf` x) filePtrs.filePathList'paths
    songs <-
      files `forM` \song ->
        lift2 $ loadMusicStream song appState._window
    songConfigs <-
      (songs `zip` [0 ..])
        `forM` \(song, i) -> do
          musicDuration <- lift2 $ getMusicTimeLength song
          return
            Song
              { media = song
              , duration = floor musicDuration
              , title = formatTitle $ files !! i
              }
    lift2 $ do
      putStrLn "INFO: Loading files:" >> print `mapM_` (title <$> songConfigs)
      return $ appState & mediaFiles %~ (++ songConfigs)
  maybe ask return appState
 where
  formatTitle xs =
    if '/' `elem` xs
      then case dropWhile (/= '/') xs of
        '/' : rest -> formatTitle rest
        x -> x
      else take (length xs - 4) xs

lift2 = lift . lift
for = flip map

mainLoop :: AppState -> IO AppState
mainLoop appState = do
  beginDrawing
  appState' <- runReaderT loop appState
  appState'' <- runReaderT appControlAudio appState'
  appState''' <- runReaderT shouldCycleNextSong appState''
  let currentSongs = appState'''._mediaFiles
  unless (null currentSongs) $ updateMusicStream (head currentSongs).media
  endDrawing
  runReaderT playMusic appState'''
 where
  shouldCycleNextSong :: AppStateIO
  shouldCycleNextSong = do
    -- unloadMusicStream
    shouldGoNext <- lift $ isKeyPressed KeyDown
    shouldGoPrev <- lift $ isKeyPressed KeyUp
    appState <- ask
    if
      | shouldGoNext && appState._mode == AppModeMediaControl -> do
          let shiftedMedia = nextCycle appState._mediaFiles
          lift $ if null shiftedMedia then return () else playMusicStream (head shiftedMedia).media
          return $ (appState & mediaFiles %~ const shiftedMedia) & counter %~ const 0
      | shouldGoPrev && appState._mode == AppModeMediaControl -> do
          let shiftedMedia = previousCycle appState._mediaFiles
          lift $ if null shiftedMedia then return () else playMusicStream (head shiftedMedia).media
          return $ (appState & mediaFiles %~ const shiftedMedia) & counter %~ const 0
      | otherwise -> return appState
  playMusic :: AppStateIO
  playMusic = do
    appState <- runMaybeT $ do
      appState <- lift ask
      guard (not . null $ appState._mediaFiles)
      lift2 $
        if
          | not appState._shouldPlay -> return appState
          | appState._counter > (head appState._mediaFiles).duration * 60 -> do
              return $ (appState & counter %~ const 0) & mediaFiles %~ pop
          | appState._counter == 0 -> do
              playMusicStream (head appState._mediaFiles).media
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
    shouldSeekAhead <- lift $ isKeyDown KeyRight
    shouldSeekBack <- lift $ isKeyDown KeyLeft
    let seekTime = 50 :: Integer
        currentTime = appState._counter
    appState <- ask
    let currentMediaFiles = appState._mediaFiles
        shouldPlayNow = appState._shouldPlay
        isInsideMedaControl = appState._mode == AppModeMediaControl
        thereAreFiles = not $ null currentMediaFiles
        currentlyPlaying = case currentMediaFiles of x : _ -> x.media; _ -> error "Unreachable pattern"
    if
      | shouldCycleAudio && shouldPlayNow && thereAreFiles -> do
          lift $ pauseMusicStream currentlyPlaying
          return $ appState & shouldPlay %~ not
      | shouldCycleAudio && not shouldPlayNow && thereAreFiles -> do
          lift $ resumeMusicStream currentlyPlaying
          return $ appState & shouldPlay %~ not
      | shouldCycleAudio -> return $ appState & shouldPlay %~ not
      | shouldSeekAhead && isInsideMedaControl && thereAreFiles -> do
          lift (seekMusicStream currentlyPlaying (fromIntegral $ currentTime `div` 60))
          return $ appState & counter %~ (+ seekTime)
      | shouldSeekBack && isInsideMedaControl && thereAreFiles -> do
          lift (seekMusicStream currentlyPlaying (fromIntegral $ currentTime `div` 60))
          return $ appState & counter %~ (\x -> if x > 0 then x - seekTime else x)
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
