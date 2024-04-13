{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

module Main where

import Control.Lens (makeLenses, makeLensesFor)
import Control.Monad (when)
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
  windowShouldClose, c'loadDroppedFiles, c'unloadDroppedFiles,
 )
import Raylib.Core.Text (drawText)
import Raylib.Core.Textures (drawTexturePro, drawTextureRec, loadTexture)
import Raylib.Types (
  Color (Color),
  FilePathList (filePathList'paths),
  KeyboardKey (KeyA, KeyD, KeyDown, KeyEnter, KeyLeft, KeyNull, KeyRight, KeyS, KeySpace, KeyT, KeyUp, KeyW, KeyX),
  Rectangle (Rectangle, rectangle'height, rectangle'width, rectangle'x, rectangle'y),
  Texture (Texture),
  Vector2 (Vector2, vector2'x, vector2'y),
 )
import Raylib.Util (WindowResources, raylibApplication, whileWindowOpen, whileWindowOpen_)
import Raylib.Util.Colors qualified as Colors
import System.Exit (exitSuccess)

import Audio (scanAudio)
import Raylib.Core.Audio (initAudioDevice, isAudioDeviceReady, loadSound, playSound)
import Foreign (Storable(peek))

default (Int)

whenIO :: IO Bool -> IO () -> IO ()
whenIO condition action = do condition' <- condition; when condition' action

class Cycle a where
  nextCycle :: a -> a
  previousCycle :: a -> a

--

$(makeLenses ''Rectangle)
makeLensesFor
  [ ("rectangleX", "_rectangleX")
  , ("rectangleY", "_rectangleY")
  , ("rectangleHeight", "_rectangleHeight")
  , ("rectangleWidth", "_rectangleWidth")
  ]
  ''Rectangle

data AppMode = AppModeOne | AppModeMenu | AppModeMediaControl deriving(Eq)

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
  { mode :: AppMode
  , txtPrompt :: Int
  , mediaFiles :: [String]
  , backGround :: Texture
  , menu :: AppMenu
  , window :: WindowResources
  }

defaultState :: WindowResources -> Texture -> AppState
defaultState w backGroundTexture =
  AppState
    { mode = AppModeOne
    , txtPrompt = 0
    , mediaFiles = []
    , backGround = backGroundTexture
    , menu = Quit
    , window = w
    }

initApp :: IO AppState
initApp = do
  w <- initWindow 1200 800 "Pokiclone"
  setTargetFPS 60
  initAudioDevice
  setExitKey KeyNull
  backGroundTexture <- loadTexture "./resources/backGround.png" w
  return $ defaultState w backGroundTexture

appModeOne :: AppState -> IO AppState
appModeOne appState = do
  clearBackground $ Color 1 20 40 1
  appState' <- refreshTextArea appState
  drawText "POG" 50 50 40 Colors.rayWhite
  appState'' <- readPrompt appState'
  drawTxtFromPrompt appState''.txtPrompt Colors.black prompt
  return appState''
 where
  refreshTextArea :: AppState -> IO AppState
  refreshTextArea appState = do
    shouldRefresh <- isKeyPressed KeyT
    if shouldRefresh
      then return appState{txtPrompt = 0}
      else return appState

  readPrompt :: AppState -> IO AppState
  readPrompt appState = do
    (\x -> drawTxtFromPrompt (length $ concat x) Colors.rayWhite (concat x)) =<< scanAudio
    let existMorePrompts = length prompt >= appState.txtPrompt + 1
    shouldGoNext <- isKeyPressed KeyA
    if shouldGoNext && existMorePrompts
      then return $ appState{txtPrompt = appState.txtPrompt + 1}
      else return appState

  drawTxtFromPrompt :: Int -> Color -> [String] -> IO ()
  drawTxtFromPrompt n color txt =
    when (n /= 0) $
      mapM_
        ( \i ->
            drawText
              (txt !! i)
              50 -- x-axis
              (100 + 45 * i) -- y-axis
              24 -- size
              color
        )
        [0 .. n - 1]

  prompt :: [String]
  prompt =
    [ "hello"
    , "Get ready to follow this wonderful adventure with me!"
    , "See this cool GUI"
    , "Made with the superpower of raylib and haskell"
    ]

appModeMenu :: AppState -> IO AppState
appModeMenu appState = do
  clearBackground $ Color 1 20 40 1
  drawText (show Quit) 50 (100 + 45) 30 Colors.rayWhite
  drawText (show Volume) 50 (100 + 45 * 2) 30 Colors.rayWhite
  drawText (show Brightness) 50 (100 + 45 * 3) 30 Colors.rayWhite

  shouldMoveDown <- isKeyPressed KeyDown
  shouldMoveUp <- isKeyPressed KeyUp
  let
    appState' =
      if
        | shouldMoveDown -> appState{menu = nextCycle appState.menu}
        | shouldMoveUp -> appState{menu = previousCycle appState.menu}
        | otherwise -> appState
  whenIO ((&& appState'.menu == Quit) <$> isKeyPressed KeyEnter) $
    closeWindow appState'.window
      >> exitSuccess
  case appState'.menu of
    Quit -> drawText (show Quit) 50 (100 + 45) 30 Colors.red >> return appState'
    Volume -> drawText (show Volume) 50 (100 + 45 * 2) 30 Colors.red >> return appState'
    Brightness -> drawText (show Brightness) 50 (100 + 45 * 3) 30 Colors.red >> return appState'

appModeMediaControl :: AppState -> IO AppState
appModeMediaControl appState = do
  clearBackground (Color 10 60 90 10)
  drawText "Media Control Panel" 50 50 40 Colors.rayWhite
  drawText (show appState.mediaFiles) 50 (100 + 45) 40 Colors.rayWhite
  isFileDropped >>= \case
    True -> do
      filePtr <- c'loadDroppedFiles
      fileContent <- peek filePtr
      c'unloadDroppedFiles filePtr
      song <- loadSound (head (fileContent.filePathList'paths)) (appState.window)
      playSound song
      return appState{mediaFiles = mediaFiles appState ++ [show fileContent.filePathList'paths]}
    False -> return appState 

mainLoop :: AppState -> IO AppState
mainLoop appState = do
  appState' <- switchMode appState
  appState'' <- case appState'.mode of
    AppModeOne -> appModeOne appState'
    AppModeMenu -> appModeMenu appState'
    AppModeMediaControl -> appModeMediaControl appState'
  endDrawing
  return appState''
 where
  switchMode :: AppState -> IO AppState
  switchMode appState = do
    shouldGoNext <- isKeyPressed KeySpace
    if shouldGoNext
      then return (appState{mode = nextCycle appState.mode})
      else return appState

shouldClose :: AppState -> IO Bool
shouldClose = const windowShouldClose

teardown :: AppState -> IO ()
teardown s = closeWindow (window s)

$(raylibApplication 'initApp 'mainLoop 'shouldClose 'teardown)

{-- 
 - Create a function to display a list on screen
 - Create a function to play music consecutively
 --}
