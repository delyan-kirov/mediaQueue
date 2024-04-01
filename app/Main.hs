{-# LANGUAGE DuplicateRecordFields #-}
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
  initWindow,
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

$(makeLenses ''Rectangle)
makeLensesFor
  [ ("rectangleX", "_rectangleX")
  , ("rectangleY", "_rectangleY")
  , ("rectangleHeight", "_rectangleHeight")
  , ("rectangleWidth", "_rectangleWidth")
  ]
  ''Rectangle

data GameState = GameState
  { _gameMode :: IORef Int
  , _entity :: IORef Rectangle
  }

$(makeLenses ''GameState)

initGameState :: IO GameState
initGameState = do
  gameModeRef <- newIORef 1
  rectangleRef <- newIORef $ Rectangle 30.0 30.0 50.0 50.0
  return $
    GameState
      { _gameMode = gameModeRef
      , _entity = rectangleRef
      }

moveDirection :: Rectangle -> IO Rectangle
moveDirection r = do
  getPressedKey <- getKeyPressed
  case getPressedKey of
    x | x `elem` [KeyA, KeyLeft] -> return $ r{rectangle'x = r.rectangle'x - 50.0}
    x | x `elem` [KeyW, KeyUp] -> return $ r{rectangle'y = r.rectangle'y - 50.0}
    x | x `elem` [KeyD, KeyRight] -> return $ r{rectangle'x = r.rectangle'x + 50.0}
    x | x `elem` [KeyS, KeyDown] -> return $ r{rectangle'y = r.rectangle'y + 50.0}
    _ -> return r

switchMode :: IORef Int -> IO ()
switchMode gameMode' = do
  shouldGoNext <- isKeyPressed KeySpace
  when shouldGoNext $ modifyIORef gameMode' (\x -> (x + 1) `mod` 3)

gameloop :: GameState -> IO ()
gameloop gameState = do
  beginDrawing

  switchMode (gameState ^. gameMode)
  currMode <- readIORef (gameState ^. gameMode)
  case currMode of
    1 -> do
      clearBackground $ Color 100 100 100 1
      drawText "POG" 50 50 40 Colors.rayWhite
    2 -> do
      -- update position
      oldRectangle <- readIORef (gameState ^. entity)
      newRectangle <- moveDirection oldRectangle
      writeIORef (gameState ^. entity) newRectangle
      -- draw
      clearBackground $ Color 120 180 240 1
      drawRectangleRec newRectangle Colors.pink
    _ -> do
      clearBackground $ Color 100 100 200 1
      drawText "Basic raylib window" 50 50 40 Colors.rayWhite
  endDrawing

main :: IO ()
main = do
  raylib <- initWindow 600 450 "TEST RAYLIB FROM HASKELL"
  setTargetFPS 60
  setExitKey KeyNull
  gameState <- initGameState

  whileWindowOpen0 $ do
    gameloop gameState

  closeWindow raylib
