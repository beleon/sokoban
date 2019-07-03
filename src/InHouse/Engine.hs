{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module InHouse.Engine (playUsingSdl) where

import qualified SDL
import Data.Maybe
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Word (Word32)
import InHouse.Resources
import Foreign.C.Types as FT
import Sokoban

newtype GameLoop a = GameLoop (ReaderT Config (StateT GameState IO) a)
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadState GameState, MonadIO)

data Config = Config
  { cWindow :: SDL.Window
  , cRenderer :: SDL.Renderer
  , cResources :: Resources
  }

data GameState = GameState
  { sokoban :: Maybe Sokoban
  , animationInfo  :: [AnimationInfo]
  }

fps :: Int
fps = 60

-- frameDuration in milliseconds
frameDuration :: Word32
frameDuration = 1000 `div` fromIntegral fps

playUsingSdl :: IO ()
playUsingSdl = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "Sokoban" SDL.defaultWindow { SDL.windowInitialSize = SDL.V2 1280 720, SDL.windowHighDPI = True, SDL.windowResizable = True}
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  resources <- loadResources renderer
  let cfg = Config
        { cWindow = window
        , cRenderer = renderer
        , cResources = resources
        }
  runGame cfg (GameState Nothing []) $ GameLoop mainLoop
  SDL.destroyWindow window
  SDL.destroyRenderer renderer
  SDL.quit

runGame :: Config -> GameState -> GameLoop a -> IO a
runGame cfg state (GameLoop a) = evalStateT (runReaderT a cfg) state

mainLoop ::
  ( MonadReader Config m
  , MonadState GameState m
  , MonadIO m
  ) => m ()
mainLoop = do
  -- start frame
  startTicks <- SDL.ticks

  -- get events
  events <- map SDL.eventPayload <$> SDL.pollEvents
  let keyevents = mapMaybe (\x -> case x of {(SDL.KeyboardEvent a) -> Just a; _ -> Nothing}) events

  -- update

  -- renderer
  renderer <- asks cRenderer
  resources <- asks cResources
  SDL.rendererDrawColor renderer SDL.$= SDL.V4 255 255 255 0
  _ <- SDL.clear renderer

  -- demo rendering
  drawSprite renderer resources "top_left_walls" $ Rect 72 136 64 64
  drawSprite renderer resources "top_walls" $ Rect 136 136 64 64
  drawSprite renderer resources "top_walls" $ Rect 200 136 64 64
  drawSprite renderer resources "top_walls" $ Rect 264 136 64 64
  drawSprite renderer resources "top_right_walls" $ Rect 328 136 64 64
  drawSprite renderer resources "left_walls" $ Rect 72 200 64 64
  drawSprite renderer resources "floor_tiles" $ Rect 136 200 64 64
  drawSprite renderer resources "floor_tiles" $ Rect 264 200 64 64
  drawSprite renderer resources "right_walls" $ Rect 328 200 64 64
  drawSprite renderer resources "boxes" $ Rect 200 200 64 64
  drawSprite renderer resources "boxes" $ Rect 200 264 64 64
  drawSprite renderer resources "bottom_left_walls" $ Rect 72 264 64 64
  drawSprite renderer resources "bottom_right_walls" $ Rect 328 264 64 64
  drawSprite renderer resources "bottom_walls" $ Rect 136 264 64 64
  drawSprite renderer resources "bottom_walls" $ Rect 264 264 64 64
  SDL.present renderer

  -- loop
  let quit = any (\x -> SDL.keyboardEventKeyMotion x == SDL.Pressed && SDL.keysymKeycode (SDL.keyboardEventKeysym x) == SDL.KeycodeQ) keyevents || SDL.QuitEvent `elem` events
  delayNextFrame startTicks
  unless quit mainLoop

delayNextFrame startTicks = do
  endTicks <- SDL.ticks
  let msUntilNextFrame = frameDuration - (endTicks - startTicks)
  when (msUntilNextFrame > 0) $ SDL.delay msUntilNextFrame
