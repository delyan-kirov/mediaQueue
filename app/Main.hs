{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS -Wall #-}

module Main (main) where

import Control.Lens (makeLenses, makeLensesFor)
import Control.Monad (when)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Raylib.Core (
  beginDrawing,
  clearBackground,
  closeWindow,
  endDrawing,
  getScreenHeight,
  getScreenWidth,
  initWindow,
  isKeyDown,
  isKeyPressed,
  setExitKey,
  setTargetFPS,
  windowShouldClose,
 )
import Raylib.Core.Text (drawText)
import Raylib.Core.Textures (drawTexturePro, drawTextureRec, loadTexture)
import Raylib.Types (
  Color (Color),
  KeyboardKey (KeyA, KeyD, KeyDown, KeyEnter, KeyLeft, KeyNull, KeyRight, KeyS, KeySpace, KeyT, KeyUp, KeyW, KeyX),
  Rectangle (Rectangle, rectangle'height, rectangle'width, rectangle'x, rectangle'y),
  Texture (Texture),
  Vector2 (Vector2, vector2'x, vector2'y),
 )
import Raylib.Util (WindowResources, whileWindowOpen, whileWindowOpen_)
import Raylib.Util.Colors qualified as Colors
import System.Exit (exitSuccess)

import Audio (scanAudio)

default (Int)

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

data GameMode = GameModeOne | GameModeTwo | GameModeThree

instance Cycle GameMode where
  nextCycle gameMode = case gameMode of
    GameModeOne -> GameModeTwo
    GameModeTwo -> GameModeThree
    GameModeThree -> GameModeOne

  previousCycle = nextCycle . nextCycle

data GameMenu = Volume | Brightness | Quit deriving (Show, Eq)
instance Cycle GameMenu where
  nextCycle gameMenu = case gameMenu of
    Volume -> Brightness
    Brightness -> Quit
    Quit -> Volume

  previousCycle = nextCycle . nextCycle

data AppState = AppState
  { mode :: GameMode
  , entity :: Vector2
  , txtPrompt :: Int
  , backGround :: Texture
  , menu :: GameMenu
  , window :: WindowResources
  }

defaultState :: WindowResources -> Texture -> AppState
defaultState w backGroundTexture =
  AppState
    { mode = GameModeOne
    , entity = Vector2 0.0 0.0
    , txtPrompt = 0
    , backGround = backGroundTexture
    , menu = Quit
    , window = w
    }

initApp :: IO AppState
initApp = do
  w <- initWindow 1200 1200 "Pokiclone"
  setTargetFPS 60
  setExitKey KeyNull
  backGroundTexture <- loadTexture "./resources/backGround1.png" w
  return $ defaultState w backGroundTexture

moveDirection :: Vector2 -> IO Vector2
moveDirection direction = do
  goLeft <- (||) <$> isKeyDown KeyA <*> isKeyDown KeyLeft
  goRight <- (||) <$> isKeyDown KeyD <*> isKeyDown KeyRight
  goDown <- (||) <$> isKeyDown KeyS <*> isKeyDown KeyDown
  goUp <- (||) <$> isKeyDown KeyW <*> isKeyDown KeyUp
  screenWidth <- getScreenWidth
  screenHeight <- getScreenHeight
  let moveAmount = 8.0
      boundaryThreshold = 100.0
  let newX =
        if
          | goLeft && direction.vector2'x - moveAmount > 0 ->
              direction.vector2'x - moveAmount
          | goRight && direction.vector2'x + moveAmount < fromIntegral screenWidth - boundaryThreshold ->
              direction.vector2'x + moveAmount
          | otherwise -> direction.vector2'x
      newY =
        if
          | goUp && direction.vector2'y - moveAmount > 0 ->
              direction.vector2'y - moveAmount
          | goDown && direction.vector2'y + moveAmount < fromIntegral screenHeight - boundaryThreshold ->
              direction.vector2'y + moveAmount
          | otherwise -> direction.vector2'y
      newPosition = direction{vector2'x = newX, vector2'y = newY}
  if newX /= direction.vector2'x || newY /= direction.vector2'y
    then return newPosition
    else return direction

gameLoopModeOne :: AppState -> IO AppState
gameLoopModeOne appState = do
  clearBackground $ Color 50 160 160 1
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
    (\x -> drawTxtFromPrompt (length . concat $ x) Colors.rayWhite (concat x)) =<< scanAudio
    let existMorePrompts = length prompt >= appState.txtPrompt + 1
    shouldGoNext <- isKeyPressed KeyA
    if shouldGoNext && existMorePrompts
      then do
        return $ appState{txtPrompt = appState.txtPrompt + 1}
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
              30 -- size
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

gameLoopModeTwo :: AppState -> IO AppState
gameLoopModeTwo appState = do
  -- update position
  newPosition <- moveDirection appState.entity
  -- draw
  clearBackground $ Color 1 20 40 1
  drawTextureRec
    appState.backGround
    Rectangle{rectangle'x = 0.0, rectangle'y = 0.0, rectangle'width = 100.0, rectangle'height = 100.0}
    Vector2{vector2'x = newPosition.vector2'x, vector2'y = newPosition.vector2'y}
    Colors.pink
  return $ appState{entity = newPosition}

gameLoopModeMenu :: AppState -> IO AppState
gameLoopModeMenu appState = do
  clearBackground $ Color 100 100 200 0
  screenWidth <- (+ 10) <$> getScreenWidth
  screenHeight <- (+ 10) <$> getScreenHeight
  drawTexturePro
    appState.backGround
    Rectangle
      { rectangle'x = 0.0
      , rectangle'y = 0.0
      , rectangle'width = 600.0
      , rectangle'height = 600.0
      }
    Rectangle
      { rectangle'x = 0.0
      , rectangle'y = 0.0
      , rectangle'width = fromIntegral screenWidth
      , rectangle'height = fromIntegral screenHeight
      }
    Vector2
      { vector2'x = 2.3
      , vector2'y = 2.3
      }
    0.1
    Colors.rayWhite
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
  makeAction <- isKeyPressed KeyEnter
  when (appState'.menu == Quit && makeAction) $
    closeWindow appState'.window
      >> exitSuccess
  case appState'.menu of
    Quit -> drawText (show Quit) 50 (100 + 45) 30 Colors.red >> return appState'
    Volume -> drawText (show Volume) 50 (100 + 45 * 2) 30 Colors.red >> return appState'
    Brightness -> drawText (show Brightness) 50 (100 + 45 * 3) 30 Colors.red >> return appState'

mainloop :: AppState -> IO AppState
mainloop appState = do
  beginDrawing

  appState' <- switchMode appState
  appState'' <- case appState'.mode of
    GameModeOne -> gameLoopModeOne appState'
    GameModeTwo -> gameLoopModeTwo appState'
    GameModeThree -> gameLoopModeMenu appState'

  endDrawing
  return appState''
 where
  switchMode :: AppState -> IO AppState
  switchMode appState = do
    shouldGoNext <- isKeyPressed KeySpace
    if shouldGoNext
      then return (appState{mode = nextCycle appState.mode})
      else return appState

main = whileWindowOpen_ mainloop =<< initApp
