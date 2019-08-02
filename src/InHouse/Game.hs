module InHouse.Game where

import qualified SDL
import           InHouse.Frame
import           InHouse.Resources

data Update a = Update
  { quit :: Bool
  , game' :: a
  }

class Game a where
  titleText :: a -> String
  update :: a -> [SDL.Event] -> IO (Update a)
  frame :: a -> Frame
