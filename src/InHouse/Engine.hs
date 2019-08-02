{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module InHouse.Engine
  ( playUsingSdl
  )
where

import qualified SDL
import           Data.Maybe
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Word                      ( Word32 )
import           InHouse.Resources
import           Foreign.C.Types               as FT
import           InHouse.Game
import           InHouse.Frame
import           InHouseSokoban

newtype GameLoop a = GameLoop (ReaderT Config (StateT InHouseSokoban IO) a)
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadState InHouseSokoban, MonadIO)

data Config = Config
  { cWindow :: SDL.Window
  , cRenderer :: SDL.Renderer
  , cResources :: Resources
  }

fps :: Int
fps = 60

-- frameDuration in milliseconds
frameDuration :: Word32
frameDuration = 1000 `div` fromIntegral fps

playUsingSdl :: InHouseSokoban -> IO ()
playUsingSdl a = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow
    "Sokoban"
    SDL.defaultWindow { SDL.windowInitialSize = SDL.V2 1440 720
                      , SDL.windowHighDPI     = True
                      , SDL.windowResizable   = True
                      }
  renderer  <- SDL.createRenderer window (-1) SDL.defaultRenderer
  resources <- loadResources renderer
  let
    cfg =
      Config { cWindow = window, cRenderer = renderer, cResources = resources }
  runGame cfg a $ GameLoop mainLoop
  SDL.destroyWindow window
  SDL.destroyRenderer renderer
  SDL.quit

runGame :: Config -> InHouseSokoban -> GameLoop a -> IO a
runGame cfg state (GameLoop a) = evalStateT (runReaderT a cfg) state

mainLoop :: (Game a, MonadReader Config m, MonadState a m, MonadIO m) => m ()
mainLoop = do
  -- start frame
  startTicks          <- SDL.ticks

  -- get events
  events              <- map SDL.eventPayload <$> SDL.pollEvents

  -- update
  game                <- get
  (Update quit game') <- liftIO $ update game events
  put game'

  -- render
  renderer  <- asks cRenderer
  resources <- asks cResources
  SDL.rendererDrawColor renderer SDL.$= SDL.V4 255 255 255 0
  _ <- SDL.clear renderer
  drawFrame renderer resources $ frame game'
  SDL.present renderer

  -- loop
  unless quit (delayNextFrame startTicks >> mainLoop)

delayNextFrame startTicks = do
  endTicks <- SDL.ticks
  let lastFrameDuration = endTicks - startTicks
      msUntilNextFrame  = frameDuration - lastFrameDuration
  when (lastFrameDuration < frameDuration) $ SDL.delay msUntilNextFrame
