{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant ^." #-}

module Main where

import Control.Lens (makeLenses, (%~), (&), (.~), (^.))
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
import Raylib.Core.Text (drawTextEx, loadFont, c'loadFontEx)
import Raylib.Types (
  Color (Color),
  FilePathList (filePathList'paths),
  Font,
  KeyboardKey (KeyDown, KeyEnter, KeyGrave, KeyLeft, KeyNull, KeyRight, KeySpace, KeyUp),
  Music,
  Rectangle (Rectangle),
  Texture (texture'height, texture'width),
  Vector2 (Vector2),
 )
import Raylib.Util (WindowResources, raylibApplication)
import Raylib.Util.Colors qualified as Colors

import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT), withReaderT)
import Data.List (isSuffixOf)
import Foreign (Storable (peek), nullPtr)
import Foreign.C (newCString)
import Raylib.Core.Audio (getMasterVolume, getMusicTimeLength, initAudioDevice, isMusicStreamPlaying, loadMusicStream, pauseMusicStream, playMusicStream, resumeMusicStream, seekMusicStream, setMasterVolume, updateMusicStream)
import Raylib.Core.Textures (drawTexturePro, loadTexture)

default (Int)

-- Data

class Cycle a where
  nextCycle :: a -> a
  previousCycle :: a -> a

instance Cycle [a] where
  nextCycle [] = []
  nextCycle (x : xs) = xs ++ [x]
  previousCycle [] = []
  previousCycle xs = last xs : init xs

-- ffmpeg -i input.flac -ab 320k -map_metadata 0 -id3v2_version 3 output.mp3

data Song = Song
  { media :: Music
  , title :: String
  , duration :: Integer
  }
  deriving (Show)

data AppMode = AppModeMenu | AppModeMediaControl deriving (Eq)

instance Cycle AppMode where
  nextCycle appMode = case appMode of
    AppModeMenu -> AppModeMediaControl
    AppModeMediaControl -> AppModeMenu

  previousCycle = nextCycle . nextCycle

data AppMenu = Volume | Brightness | Quit deriving (Show, Eq)
instance Cycle AppMenu where
  nextCycle appMenu = case appMenu of
    Volume -> Brightness
    Brightness -> Quit
    Quit -> Volume

  previousCycle = nextCycle . nextCycle

data Effect = Effect {_effDuration :: Integer, _current :: Integer, _isOn :: Bool}
makeLenses ''Effect

data Effects = Effects
  { _songCounter :: Integer
  , _pauseEff :: Effect
  , _resumeEff :: Effect
  , _playEff :: Effect
  , _seekForthEff :: Effect
  , _seekBackEff :: Effect
  }

data AppIcons = AppIcons
  { _iconPlay :: Texture
  , _iconPause :: Texture
  , _iconLeftArrow :: Texture
  , _iconRightArrow :: Texture
  , _iconPlayStatic :: Texture
  , _iconPauseStatic :: Texture
  , _iconLeftArrowStatic :: Texture
  , _iconRightArrowStatic :: Texture
  }

makeLenses ''AppIcons

makeLenses ''Effects

data AppState = AppState
  { _mode :: AppMode
  , _mediaFiles :: [Song]
  , _menu :: AppMenu
  , _window :: WindowResources
  , _effect :: Effects
  , _shouldPlay :: Bool
  , _appIcons :: AppIcons
  , _fonts :: [Font]
  }

makeLenses ''AppState

type AppStateIO = ReaderT AppState IO AppState

--

initApp :: IO AppState
initApp = do
  w <- initWindow 600 800 "Pokiclone"
  setTargetFPS 60
  fontReg <- loadFont' "./resources/sever-sans-font/SeverSansBook-nRRvP.ttf"
  fontBold <- loadFont "./resources/asimov-font/AsimovWide-0qrG.otf" w
  iconPlay <- loadTexture "./resources/icons/play-button.png" w
  iconPause <- loadTexture "./resources/icons/pause.png" w
  iconLeft <- loadTexture "./resources/icons/left-chevron.png" w
  iconRight <- loadTexture "./resources/icons/right-chevron.png" w
  iconResume <- loadTexture "./resources/icons/play-button.png" w
  -- Load static icons
  iconPlayStatic <- loadTexture "./resources/icons/play-button-static.png" w
  iconPauseStatic <- loadTexture "./resources/icons/pause-static.png" w
  iconLeftStatic <- loadTexture "./resources/icons/left-chevron-static.png" w
  iconRightStatic <- loadTexture "./resources/icons/right-chevron-static.png" w

  initAudioDevice
  setExitKey KeyNull
  let thisAppIcons =
        AppIcons
          { _iconPlay = iconPlay
          , _iconPause = iconPause
          , _iconLeftArrow = iconLeft
          , _iconRightArrow = iconRight
          , _iconPlayStatic = iconPlayStatic
          , _iconPauseStatic = iconPauseStatic
          , _iconLeftArrowStatic = iconLeftStatic
          , _iconRightArrowStatic = iconRightStatic
          }
      playPauseEffect =
        Effect
          { _effDuration = 30
          , _current = 0
          , _isOn = False
          }
      seekEffect =
        Effect
          { _effDuration = 10
          , _current = 0
          , _isOn = False
          }

      thisEffects =
        Effects
          { _songCounter = 0
          , _pauseEff = playPauseEffect
          , _resumeEff = playPauseEffect
          , _playEff = playPauseEffect
          , _seekBackEff = seekEffect
          , _seekForthEff = seekEffect
          }
      initialState =
        AppState
          { _mode = AppModeMediaControl
          , _mediaFiles = []
          , _menu = Quit
          , _window = w
          , _effect = thisEffects
          , _shouldPlay = True
          , _appIcons = thisAppIcons
          , _fonts = [fontReg, fontBold]
          }
  return initialState
 where
  loadFont' :: String -> IO Font
  loadFont' txt = do
    txt'c <- newCString txt
    font' <- c'loadFontEx txt'c 32 nullPtr 0
    peek font'

data TextF = TextF
  { txtString :: String -- The text content
  , xAxis :: Float -- Position on the x-axis
  , yAxis :: Float -- Position on the y-axis
  , size :: Float -- Font size
  , color :: Color -- Text color
  }

-- Helpers

lift2 = lift . lift
for = flip map

drawTextF :: TextF -> AppStateIO
drawTextF textf = do
  -- void DrawTextEx(Font font, const char *text, Vector2 position, float fontSize, float spacing, Color tint); // Draw text using font and additional parameters
  appState <- ask
  screenWidth <- lift getScreenWidth
  screenHeight <- lift getScreenHeight
  lift $
    drawTextEx
      (head appState._fonts)
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
  unless (null text) $
    [0 .. n - 1] `forM_` \i ->
      drawTextF
        ( TextF
            { txtString = text !! i
            , xAxis = 50.0
            , yAxis = 100.0 + 45.0 * fromIntegral i * (0.001 * fromIntegral screenHeight)
            , size = 100.0
            , color = color
            }
        )
  ask

iconPadding = 20

-- App modes

appModeMenu :: AppStateIO
appModeMenu = do
  lift $ clearBackground $ Color 1 20 40 1
  void $ drawTxtFromPrompt Colors.rayWhite (show <$> [Quit, Volume, Brightness])

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
  let firstSongColor = Color 255 100 250 250
      currSongs = take 12 (title <$> appState._mediaFiles)
  lift $ clearBackground $ Color 1 20 40 1
  void $
    drawTextF (TextF "Music Queue" 50 10 120 Colors.rayWhite)
      >> drawTxtFromPrompt (Color 255 200 230 255) currSongs
      >> drawTxtFromPrompt firstSongColor (getCurrentSong currSongs)
  -- Ckeck if files are dropped
  appState <- runMaybeT $ do
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
  getCurrentSong [] = []
  getCurrentSong currSongs = head currSongs : replicate (length currSongs - 1) ""

  formatTitle xs =
    if '/' `elem` xs
      then case dropWhile (/= '/') xs of
        '/' : rest -> formatTitle rest
        x -> x
      else
        let xs' = take (length xs - 4) xs
            xs'' = dropWhile (== ' ') $ drop 1 $ dropWhile (/= '-') xs'
         in if length xs'' > 35
              then take 35 xs'' ++ "..."
              else xs''

-- App structure

mainLoop :: AppState -> IO AppState
mainLoop appStateM = do
  beginDrawing
  drawStaticIcons appStateM
  appState' <- runReaderT loop appStateM
  appState'' <- runReaderT shouldCycleNextSong appState'
  appState''' <- runReaderT handleEffect appState''
  let currentSongs = appState''._mediaFiles
  unless (null currentSongs) $ updateMusicStream (head currentSongs).media
  endDrawing
  runReaderT playMusic appState'''
 where
  iconSize :: IO Int
  iconSize = do
    scrWidth <- getScreenWidth
    scrHeigt <- getScreenHeight
    return $ (scrWidth * scrHeigt) `div` 8000 + 20
  drawStaticIcons :: AppState -> IO ()
  drawStaticIcons appState = do
    scrWidth <- getScreenWidth
    scrHeight <- getScreenHeight
    recSize <- iconSize
    isAnythingPlaying <- case media <$> appState._mediaFiles of
      [] -> return False
      currSong : _ -> isMusicStreamPlaying currSong
    let
      iconPlayTexture = appState ^. appIcons ^. iconPlayStatic
      iconPauseTexture = appState ^. appIcons ^. iconPauseStatic
      iconLeftTexture = appState ^. appIcons ^. iconLeftArrowStatic
      iconRightTexture = appState ^. appIcons ^. iconRightArrowStatic
      recX'play = (scrWidth - recSize) `div` 2
      recX'left = (scrWidth - recSize) `div` 2 - (recSize + iconPadding)
      recX'right = (scrWidth - recSize) `div` 2 + (recSize + iconPadding)
      recY = (scrHeight - recSize) - iconPadding

      sourceRec =
        Rectangle
          0.0
          0.0
          (fromIntegral iconPlayTexture.texture'width)
          (fromIntegral iconPlayTexture.texture'height)

      destRec'play =
        Rectangle
          (fromIntegral recX'play)
          (fromIntegral recY)
          (fromIntegral recSize)
          (fromIntegral recSize)

      destRec'left =
        Rectangle
          (fromIntegral recX'left)
          (fromIntegral recY)
          (fromIntegral recSize)
          (fromIntegral recSize)

      destRec'right =
        Rectangle
          (fromIntegral recX'right)
          (fromIntegral recY)
          (fromIntegral recSize)
          (fromIntegral recSize)

      origin = Vector2 0.0 0.0
    if isAnythingPlaying
      then drawTexturePro iconPlayTexture sourceRec destRec'play origin 0.0 Colors.rayWhite
      else drawTexturePro iconPauseTexture sourceRec destRec'play origin 0.0 Colors.rayWhite
    drawTexturePro iconRightTexture sourceRec destRec'right origin 0.0 Colors.rayWhite
    drawTexturePro iconLeftTexture sourceRec destRec'left origin 0.0 Colors.rayWhite

  shouldCycleNextSong :: AppStateIO
  shouldCycleNextSong = do
    -- unloadMusicStream
    shouldGoNext <- lift $ isKeyPressed KeyDown
    shouldGoPrev <- lift $ isKeyPressed KeyUp
    appState <- ask
    let isInMediaControlMode = appState._mode == AppModeMediaControl
        appMediaStack = appState._mediaFiles
    if
      | shouldGoNext && isInMediaControlMode -> do
          let shiftedMedia = nextCycle appMediaStack
          lift $ unless (null shiftedMedia) $ playMusicStream (head shiftedMedia).media
          return $
            appState
              & mediaFiles %~ nextCycle
              & effect . songCounter %~ (+ 1)
              & shouldPlay .~ True
      | shouldGoPrev && isInMediaControlMode -> do
          let shiftedMedia = previousCycle appMediaStack
          lift $ unless (null shiftedMedia) $ playMusicStream (head shiftedMedia).media
          return $
            appState
              & mediaFiles %~ previousCycle
              & effect . songCounter .~ 0
              & shouldPlay .~ True
      | otherwise -> return appState

  drawPlayIcon :: AppState -> IO ()
  drawPlayIcon appState = do
    scrWidth <- getScreenWidth
    scrHeight <- getScreenHeight
    recSize <- iconSize

    let iconTexture = appState ^. appIcons ^. iconPlay
        recX = (scrWidth - recSize) `div` 2
        recY = (scrHeight - recSize) - iconPadding

        sourceRec =
          Rectangle
            0.0
            0.0
            (fromIntegral iconTexture.texture'width)
            (fromIntegral iconTexture.texture'height)

        destRec =
          Rectangle
            (fromIntegral recX)
            (fromIntegral recY)
            (fromIntegral recSize)
            (fromIntegral recSize)

        origin = Vector2 0.0 0.0
    drawTexturePro iconTexture sourceRec destRec origin 0.0 Colors.rayWhite

  drawLeftIcon :: AppState -> IO ()
  drawLeftIcon appState = do
    scrWidth <- getScreenWidth
    scrHeight <- getScreenHeight
    recSize <- iconSize

    let iconTexture = appState ^. appIcons ^. iconLeftArrow
        recX = (scrWidth - recSize) `div` 2 - (recSize + iconPadding)
        recY = (scrHeight - recSize) - iconPadding
        sourceRec =
          Rectangle
            0.0
            0.0
            (fromIntegral iconTexture.texture'width)
            (fromIntegral iconTexture.texture'height)

        destRec =
          Rectangle
            (fromIntegral recX)
            (fromIntegral recY)
            (fromIntegral recSize)
            (fromIntegral recSize)

        origin = Vector2 0.0 0.0
    drawTexturePro iconTexture sourceRec destRec origin 0.0 Colors.rayWhite

  drawRightIcon :: AppState -> IO ()
  drawRightIcon appState = do
    scrWidth <- getScreenWidth
    scrHeight <- getScreenHeight
    recSize <- iconSize

    let iconTexture = appState ^. appIcons ^. iconRightArrow
        recX = (scrWidth - recSize) `div` 2 + (recSize + iconPadding)
        recY = (scrHeight - recSize) - iconPadding
        sourceRec =
          Rectangle
            0.0
            0.0
            (fromIntegral iconTexture.texture'width)
            (fromIntegral iconTexture.texture'height)

        destRec =
          Rectangle
            (fromIntegral recX)
            (fromIntegral recY)
            (fromIntegral recSize)
            (fromIntegral recSize)

        origin = Vector2 0.0 0.0
    drawTexturePro iconTexture sourceRec destRec origin 0.0 Colors.rayWhite

  drawPauseIcon :: AppState -> IO ()
  drawPauseIcon appState = do
    scrWidth <- getScreenWidth
    scrHeight <- getScreenHeight
    recSize <- iconSize

    let iconTexture = appState ^. appIcons ^. iconPause
        recX = (scrWidth - recSize) `div` 2
        recY = (scrHeight - recSize) - iconPadding

        sourceRec =
          Rectangle
            0.0
            0.0
            (fromIntegral iconTexture.texture'width)
            (fromIntegral iconTexture.texture'height)

        destRec =
          Rectangle
            (fromIntegral recX)
            (fromIntegral recY)
            (fromIntegral recSize)
            (fromIntegral recSize)

        origin = Vector2 0.0 0.0
    drawTexturePro iconTexture sourceRec destRec origin 0.0 Colors.rayWhite

  playMusic :: AppStateIO
  playMusic = do
    shouldCycleAudio <- lift $ isKeyPressed KeySpace
    shouldSeekAhead <- lift $ isKeyDown KeyRight
    shouldSeekBack <- lift $ isKeyDown KeyLeft
    appState <- runMaybeT $ do
      appState <- lift ask
      guard (not . null $ appState._mediaFiles)
      let
        currentSong = head appState._mediaFiles
        -- isPlaying = appState._shouldPlay -- use if you want to keep music stopped
        seekTime = 50
        currentTime = appState._effect._songCounter
        currentMediaFiles = appState._mediaFiles
        shouldPlayNow = appState._shouldPlay
        isInsideMedaControl = appState._mode == AppModeMediaControl
        thereAreFiles = not $ null currentMediaFiles
        initNewSong = appState._effect._songCounter == 0
        endCurrSong = appState._effect._songCounter > currentSong.duration * 60
        currentlyPlaying = case currentMediaFiles of x : _ -> x.media; _ -> error "Unreachable pattern"
      lift2 $
        if
          | endCurrSong ->
              return $
                appState
                  & effect . songCounter .~ 0
                  & mediaFiles %~ pop
          | initNewSong -> do
              playMusicStream currentSong.media
              return $
                appState
                  & effect . songCounter %~ (+ 1)
          | shouldCycleAudio && shouldPlayNow && thereAreFiles -> do
              pauseMusicStream currentlyPlaying
              return $
                appState
                  & shouldPlay %~ not
                  & effect . pauseEff . isOn .~ True
          | shouldCycleAudio && not shouldPlayNow && thereAreFiles -> do
              resumeMusicStream currentlyPlaying
              return $
                appState
                  & shouldPlay %~ not
                  & effect . resumeEff . isOn .~ True
          | shouldCycleAudio -> return $ appState & shouldPlay %~ not
          | shouldSeekAhead && isInsideMedaControl && thereAreFiles -> do
              seekMusicStream currentlyPlaying (fromIntegral $ currentTime `div` 60)
              return $
                appState
                  & effect . songCounter %~ (+ seekTime)
                  & effect . seekForthEff . isOn .~ True
          | shouldSeekBack && isInsideMedaControl && thereAreFiles -> do
              seekMusicStream currentlyPlaying (fromIntegral $ currentTime `div` 60)
              return $
                appState
                  & effect . songCounter %~ (\x -> if x > 0 then x - seekTime else x)
                  & effect . seekBackEff . isOn .~ True
          | otherwise -> return $ appState & effect . songCounter %~ (+ 1)
    maybe ask return appState

  loop :: AppStateIO = do
    appState <- switchMode
    withReaderT (const appState) $ case appState._mode of
      AppModeMenu -> appModeMenu
      AppModeMediaControl -> appModeMediaControl

  handleEffect :: AppStateIO = do
    appState <- ask
    {-- Lenses make this code unfactorable basically
     -- you cannot abstract the effect
     -- the attemp of doing so makes the type checker go crazy --}
    let playPauseEff =
          appState ^. effect . pauseEff . isOn
            && appState ^. effect . pauseEff . effDuration
              > appState ^. effect . pauseEff . current
        stopPauseEff =
          appState ^. effect . pauseEff . effDuration
            <= appState ^. effect . pauseEff . current
        --
        playResumeEff =
          appState ^. effect . resumeEff . isOn
            && appState ^. effect . resumeEff . effDuration
              > appState ^. effect . resumeEff . current
        stopResumeEff =
          appState ^. effect . resumeEff . effDuration
            <= appState ^. effect . resumeEff . current
        --
        playRightArrowEff =
          appState ^. effect . seekForthEff . isOn
            && appState ^. effect . seekForthEff . effDuration
              > appState ^. effect . seekForthEff . current
        stopRightArrowEff =
          appState ^. effect . seekForthEff . effDuration
            <= appState ^. effect . seekForthEff . current
        --
        playLeftArrowEff =
          appState ^. effect . seekBackEff . isOn
            && appState ^. effect . seekBackEff . effDuration
              > appState ^. effect . seekBackEff . current
        stopLeftArrowEff =
          appState ^. effect . seekBackEff . effDuration
            <= appState ^. effect . seekBackEff . current

    lift $
      if
        | playPauseEff && playResumeEff -> do
            -- prevent spam
            return $
              appState
                & effect . pauseEff . current %~ (+ 1)
                & effect . resumeEff . current %~ (+ 1)
        | playPauseEff -> do
            drawPauseIcon appState
            return $
              appState
                & effect . pauseEff . current %~ (+ 1)
        | stopPauseEff -> do
            return $
              appState
                & effect . pauseEff . isOn %~ not
                & effect . pauseEff . current .~ 0
        | playRightArrowEff -> do
            drawRightIcon appState
            return $
              appState
                & effect . seekForthEff . current %~ (+ 1)
        | stopRightArrowEff -> do
            return $
              appState
                & effect . seekForthEff . isOn %~ not
                & effect . seekForthEff . current .~ 0
        | playLeftArrowEff -> do
            drawLeftIcon appState
            return $
              appState
                & effect . seekBackEff . current %~ (+ 1)
        | stopLeftArrowEff -> do
            return $
              appState
                & effect . seekBackEff . isOn %~ not
                & effect . seekBackEff . current .~ 0
        | playResumeEff -> do
            drawPlayIcon appState
            return $
              appState
                & effect . resumeEff . current %~ (+ 1)
        | stopResumeEff ->
            return $
              appState
                & effect . resumeEff . isOn %~ not
                & effect . resumeEff . current .~ 0
        | otherwise -> return appState

  switchMode :: AppStateIO
  switchMode = do
    shouldGoNext <- lift $ isKeyPressed KeyGrave
    appState <- ask
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
-- unload textures
teardown appState = closeWindow appState._window

$(raylibApplication 'initApp 'mainLoop 'shouldClose 'teardown)
