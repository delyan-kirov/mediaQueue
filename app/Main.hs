{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Lens (makeLenses, makeLensesFor, (^.))
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
 )
import Raylib.Core.Text (drawText)
import Raylib.Core.Textures (drawTexturePro, drawTextureRec, loadTexture, unloadTexture)
import Raylib.Types (
  Color (Color),
  KeyboardKey (KeyA, KeyD, KeyDown, KeyLeft, KeyNull, KeyRight, KeyS, KeySpace, KeyT, KeyUp, KeyW),
  Rectangle (Rectangle, rectangle'height, rectangle'width, rectangle'x, rectangle'y),
  Texture,
  Vector2 (Vector2, vector2'x, vector2'y),
 )
import Raylib.Util (WindowResources, whileWindowOpen0)
import Raylib.Util.Colors qualified as Colors

default (Int)

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

data GameState = GameState
  { _gameMode :: IORef GameMode
  , _entity :: IORef Vector2
  , _txtPrompt :: IORef Int
  , _backGround :: Texture
  }

$(makeLenses ''GameState)

nextGameMode :: GameMode -> GameMode
nextGameMode gameMode' = case gameMode' of
  GameModeOne -> GameModeTwo
  GameModeTwo -> GameModeThree
  GameModeThree -> GameModeOne

prompt :: [String]
prompt =
  [ "hello"
  , "Get ready to follow this wonderful adventure with me!"
  , "See this cool GUI"
  , "Made with the superpower of raylib and haskell"
  ]

initGameState :: WindowResources -> IO GameState
initGameState raylib = do
  gameModeRef <- newIORef GameModeOne
  rectangleRef <- newIORef $ Vector2 0.0 0.0
  txtPromptRef <- newIORef 0
  backGround' <- loadTexture "./resources/backGround.png" raylib
  return $
    GameState
      { _gameMode = gameModeRef
      , _entity = rectangleRef
      , _txtPrompt = txtPromptRef
      , _backGround = backGround'
      }

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

readPrompt :: IORef Int -> IO Int
readPrompt newTxtPrompt = do
  newTxtPrompt' <- readIORef newTxtPrompt
  let existMorePrompts = length prompt >= newTxtPrompt' + 1
  shouldGoNext <- isKeyPressed KeyA
  if shouldGoNext && existMorePrompts
    then do
      modifyIORef newTxtPrompt (+ 1)
      return $ newTxtPrompt' + 1
    else return newTxtPrompt'

drawTxtFromPrompt :: Int -> [String] -> IO ()
drawTxtFromPrompt n txt =
  when (n /= 0) $
    mapM_
      ( \i ->
          drawText
            (txt !! i)
            50 -- x-axis
            (100 + 45 * i) -- y-axis
            30 -- size
            Colors.black
      )
      [0 .. n - 1]

switchMode :: IORef GameMode -> IO ()
switchMode gameMode' = do
  shouldGoNext <- isKeyPressed KeySpace
  when shouldGoNext $ modifyIORef gameMode' nextGameMode

gameLoopModeOne :: GameState -> IO ()
gameLoopModeOne gameState = do
  clearBackground $ Color 50 160 160 1
  refreshTextArea gameState
  drawText "POG" 50 50 40 Colors.rayWhite
  newTxtPrompt <- readPrompt (gameState ^. txtPrompt)
  drawTxtFromPrompt newTxtPrompt prompt

gameLoopModeTwo :: GameState -> IO ()
gameLoopModeTwo gameState = do
  -- update position
  oldPosition <- readIORef (gameState ^. entity)
  newPosition <- moveDirection oldPosition
  writeIORef (gameState ^. entity) newPosition
  -- draw
  clearBackground $ Color 1 20 40 1
  drawTextureRec
    gameState._backGround
    Rectangle{rectangle'x = 0.0, rectangle'y = 0.0, rectangle'width = 100.0, rectangle'height = 100.0}
    Vector2{vector2'x = newPosition.vector2'x, vector2'y = newPosition.vector2'y}
    Colors.pink

gameLoopModeThree :: GameState -> IO ()
gameLoopModeThree gameState = do
  clearBackground $ Color 100 100 200 0
  screenWidth <- getScreenWidth
  screenHeight <- getScreenHeight
  drawTexturePro
    gameState._backGround
    Rectangle
      { rectangle'x = 0.0
      , rectangle'y = 0.0
      , rectangle'width = 800.0
      , rectangle'height = 800.0
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
  drawText "Basic raylib window" 50 50 40 Colors.rayWhite

refreshTextArea :: GameState -> IO ()
refreshTextArea gameState = do
  shouldRefresh <- isKeyPressed KeyT
  when shouldRefresh $ writeIORef (gameState ^. txtPrompt) 0

gameloop :: GameState -> IO ()
gameloop gameState = do
  beginDrawing

  switchMode $ gameState ^. gameMode
  currMode <- readIORef $ gameState ^. gameMode
  case currMode of
    GameModeOne -> gameLoopModeOne gameState
    GameModeTwo -> gameLoopModeTwo gameState
    GameModeThree -> gameLoopModeThree gameState
  endDrawing

main :: IO ()
main = do
  raylib <- initWindow 1000 1000 "Pokiclone"
  setTargetFPS 60
  setExitKey KeyNull
  gameState <- initGameState raylib

  whileWindowOpen0 $ do
    gameloop gameState

  unloadTexture gameState._backGround raylib
  closeWindow raylib
