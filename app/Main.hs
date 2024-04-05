{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Lens
import Control.Monad (when)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Raylib.Core (
  beginDrawing,
  clearBackground,
  closeWindow,
  endDrawing,
  getKeyPressed,
  getScreenHeight,
  getScreenWidth,
  initWindow,
  isKeyDown,
  isKeyPressed,
  setExitKey,
  setTargetFPS,
 )
import Raylib.Core.Shapes (drawRectangleRec)
import Raylib.Core.Text (drawText)
import Raylib.Types (
  Color (Color),
  KeyboardKey (KeyA, KeyD, KeyDown, KeyLeft, KeyNull, KeyRight, KeyS, KeySpace, KeyUp, KeyW),
  Rectangle (Rectangle, rectangle'height, rectangle'width, rectangle'x, rectangle'y),
 )
import Raylib.Util (whileWindowOpen0)
import Raylib.Util.Colors qualified as Colors

default (Int)

$(makeLenses ''Rectangle)
makeLensesFor
  [ ("rectangleX", "_rectangleX")
  , ("rectangleY", "_rectangleY")
  , ("rectangleHeight", "_rectangleHeight")
  , ("rectangleWidth", "_rectangleWidth")
  ]
  ''Rectangle

data GameMode = GameModeOne | GameModeTwo | GameModeThree

nextGameMode :: GameMode -> GameMode
nextGameMode GameModeOne = GameModeTwo
nextGameMode GameModeTwo = GameModeThree
nextGameMode GameModeThree = GameModeOne

data GameState = GameState
  { _gameMode :: IORef GameMode
  , _entity :: IORef Rectangle
  , _txtPrompt :: IORef Int
  }

$(makeLenses ''GameState)

prompt =
  [ "hello"
  , "Get ready to follow this wonderful adventure with me!"
  , "See this cool GUI"
  , "Made with the superpower of raylib and haskell"
  ]

initGameState :: IO GameState
initGameState = do
  gameModeRef <- newIORef GameModeOne
  rectangleRef <- newIORef $ Rectangle 30.0 30.0 100.0 100.0
  txtPromptRef <- newIORef 0
  return $
    GameState
      { _gameMode = gameModeRef
      , _entity = rectangleRef
      , _txtPrompt = txtPromptRef
      }

moveDirection :: Rectangle -> IO Rectangle
moveDirection r = do
  goLeft <- isKeyDown KeyA
  goRight <- isKeyDown KeyD
  goDown <- isKeyDown KeyS
  goUp <- isKeyDown KeyW
  screenWidth <- getScreenWidth
  screenHeight <- getScreenHeight
  let moveAmount = 10.0
      boundaryThreshold = 120.0
  let newX
        | goLeft && r.rectangle'x - moveAmount > 0 = r.rectangle'x - moveAmount
        | goRight && r.rectangle'x + moveAmount < fromIntegral screenWidth - boundaryThreshold = r.rectangle'x + moveAmount
        | otherwise = r.rectangle'x
      newY
        | goUp && r.rectangle'y - moveAmount > 0 = r.rectangle'y - moveAmount
        | goDown && r.rectangle'y + moveAmount < fromIntegral screenHeight - boundaryThreshold = r.rectangle'y + moveAmount
        | otherwise = r.rectangle'y
      newPosition = r{rectangle'x = newX, rectangle'y = newY}
  if newX /= r.rectangle'x || newY /= r.rectangle'y
    then return newPosition
    else do
      return r

readPrompt :: IORef Int -> IO Int
readPrompt newTxtPrompt = do
  newTxtPrompt' <- readIORef newTxtPrompt
  let existMorePrompts = length prompt >= newTxtPrompt' + 2
  shouldGoNext <- isKeyPressed KeyA
  if shouldGoNext && existMorePrompts
    then do
      modifyIORef newTxtPrompt (+ 1)
      return $ newTxtPrompt' + 1
    else return newTxtPrompt'

drawTxtFromPrompt :: Int -> [String] -> IO ()
drawTxtFromPrompt n txt =
  mapM_
    ( \i ->
        drawText
          (txt !! i)
          50 -- x-axis
          (100 + 45 * i) -- y-axis
          30 -- size
          Colors.black
    )
    [0 .. n]

switchMode :: IORef GameMode -> IO ()
switchMode gameMode' = do
  shouldGoNext <- isKeyPressed KeySpace
  when shouldGoNext $ modifyIORef gameMode' nextGameMode

gameloop :: GameState -> IO ()
gameloop gameState = do
  beginDrawing

  switchMode (gameState ^. gameMode)
  currMode <- readIORef (gameState ^. gameMode)
  case currMode of
    GameModeOne -> do
      clearBackground $ Color 100 100 100 1
      drawText "POG" 50 50 40 Colors.rayWhite
      newTxtPrompt <- readPrompt (gameState ^. txtPrompt)
      drawTxtFromPrompt newTxtPrompt prompt
    GameModeTwo -> do
      -- update position
      oldRectangle <- readIORef (gameState ^. entity)
      newRectangle <- moveDirection oldRectangle
      writeIORef (gameState ^. entity) newRectangle
      -- draw
      clearBackground $ Color 120 180 240 1
      drawRectangleRec newRectangle Colors.pink
    GameModeThree -> do
      clearBackground $ Color 100 100 200 1
      drawText "Basic raylib window" 50 50 40 Colors.rayWhite
  endDrawing

main :: IO ()
main = do
  raylib <- initWindow 1000 1000 "Pokiclone"
  setTargetFPS 60
  setExitKey KeyNull
  gameState <- initGameState

  whileWindowOpen0 $ do
    gameloop gameState

  closeWindow raylib
