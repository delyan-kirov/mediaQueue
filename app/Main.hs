module Main (main) where

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
import Raylib.Core.Shapes (drawRectangle, drawRectangleRec)
import Raylib.Core.Text (drawText)
import Raylib.Types (Color (Color), KeyboardKey (KeyA, KeyB, KeyC, KeyD, KeyNull, KeyS, KeySpace, KeyW), Rectangle (Rectangle, rectangle'height, rectangle'width, rectangle'x, rectangle'y))
import Raylib.Util (whileWindowOpen0)
import Raylib.Util.Colors qualified as Colors

moveDirection :: Rectangle -> IO Rectangle
moveDirection rect = do
  getPressedKey <- getKeyPressed
  case getPressedKey of
    KeyA ->
      return $
        Rectangle
          (rectangle'x rect - 50.0)
          (rectangle'y rect)
          (rectangle'height rect)
          (rectangle'width rect)
    KeyW ->
      return $
        Rectangle
          (rectangle'x rect)
          (rectangle'y rect - 50.0)
          (rectangle'height rect)
          (rectangle'width rect)
    KeyD ->
      return $
        Rectangle
          (rectangle'x rect + 50.0)
          (rectangle'y rect)
          (rectangle'height rect)
          (rectangle'width rect)
    KeyS ->
      return $
        Rectangle
          (rectangle'x rect)
          (rectangle'y rect + 50.0)
          (rectangle'height rect)
          (rectangle'width rect)
    _ -> return rect

switchMode :: IORef Int -> IO ()
switchMode gameMode = do
  shouldGoNext <- isKeyPressed KeySpace
  when shouldGoNext $ modifyIORef gameMode (\x -> (x + 1) `mod` 3)

gameloop :: IORef Int -> IORef Rectangle -> IO ()
gameloop gameMode rect1 = do
  beginDrawing

  switchMode gameMode
  currMode <- readIORef gameMode
  case currMode of
    1 -> do
      clearBackground $ Color 100 100 100 1
      drawText "POG" 50 50 40 Colors.rayWhite
    2 -> do
      oldRectangle <- readIORef rect1
      newRectangle <- moveDirection oldRectangle
      writeIORef rect1 newRectangle
      clearBackground $ Color 100 100 100 1
      drawRectangleRec newRectangle Colors.rayWhite
    _ -> do
      clearBackground $ Color 100 100 200 1
      drawText "Basic raylib window" 50 50 40 Colors.rayWhite
  endDrawing

main :: IO ()
main = do
  raylib <- initWindow 600 450 "TEST RAYLIB FROM HASKELL"
  gameModeRef <- newIORef 1
  rectangleRef <- newIORef $ Rectangle 30.0 30.0 50.0 50.0
  setTargetFPS 60
  setExitKey KeyNull

  whileWindowOpen0 $ do
    gameloop gameModeRef rectangleRef

  closeWindow raylib
