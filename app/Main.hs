module Main (main) where

import Raylib.Core ( clearBackground
                   , beginDrawing
                   , endDrawing
                   , setExitKey
                   , setTargetFPS
                   , initWindow
                   , isKeyPressed
                   , closeWindow )
import Raylib.Core.Text (drawText)
import Raylib.Util (whileWindowOpen0)
import Raylib.Core.Shapes (drawRectangle)
import Control.Monad (when)
import qualified Raylib.Util.Colors as Colors
import Raylib.Types (Color(Color), KeyboardKey (KeyNull, KeyA))
import Data.IORef (newIORef, modifyIORef, readIORef)

gameloop :: Int -> IO ()
gameloop gameMode = do
  beginDrawing
  case gameMode of
    1 -> do
      clearBackground $ Color 100 100 100 1
      drawText "POG" 50 50 40 Colors.rayWhite
    2 -> do
      clearBackground $ Color 100 100 100 1
      drawRectangle 100 30 300 300 Colors.rayWhite
    _ -> do
      clearBackground $ Color 100 100 200 1
      drawText "Basic raylib window" 50 50 40 Colors.rayWhite

  endDrawing

main :: IO ()
main = do
  raylib <- initWindow 600 450 "TEST RAYLIB FROM HASKELL"
  gameModeRef <- newIORef 1
  setTargetFPS 60
  setExitKey KeyNull

  whileWindowOpen0 $ do
    shouldGoNext <- isKeyPressed KeyA
    when shouldGoNext $ modifyIORef gameModeRef (\ x -> mod (x + 1) 3)
    currMode <- readIORef gameModeRef
    gameloop currMode

  closeWindow raylib
