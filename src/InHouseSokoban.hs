module InHouseSokoban where

import InHouse.Game
import InHouse.Frame
import InHouse.Resources
import Sokoban
import qualified SDL
import Data.Maybe

data InHouseSokoban = InHouseSokoban
  { sokoban :: Sokoban
  , animationInfo :: [AnimationInfo]
  }

instance Game InHouseSokoban where
  titleText _ = "Sokoban"
  update game events = do
    let quit = any (\x -> SDL.keyboardEventKeyMotion x == SDL.Pressed && SDL.keysymKeycode (SDL.keyboardEventKeysym x) == SDL.KeycodeQ) keyevents || SDL.QuitEvent `elem` events
        keyevents = mapMaybe (\x -> case x of {(SDL.KeyboardEvent a) -> Just a; _ -> Nothing}) events
        [ai] = animationInfo game
    now <- fmap fromIntegral SDL.ticks
    let newAlive = aiAliveTicks ai + now - aiLastUpdated ai
    return $ Update quit (game {animationInfo = [ai {aiLastUpdated = now, aiAliveTicks = newAlive}]})
  frame game = Frame $ (map (\(a,b,c) -> StillInstance a b c)
    [ ("top_left_walls", Rect 72 136 64 64, 0)
    , ("top_walls", Rect 136 136 64 64, 0)
    , ("top_walls", Rect 200 136 64 64, 0)
    , ("top_walls", Rect 264 136 64 64, 0)
    , ("top_right_walls", Rect 328 136 64 64, 0)
    , ("left_walls", Rect 72 200 64 64, 0)
    , ("floor_tiles", Rect 136 200 64 64, 0)
    , ("floor_tiles", Rect 264 200 64 64, 0)
    , ("right_walls", Rect 328 200 64 64, 0)
    , ("boxes", Rect 200 200 64 64, 0)
    , ("boxes", Rect 200 264 64 64, 0)
    , ("bottom_left_walls", Rect 72 264 64 64, 0)
    , ("bottom_right_walls", Rect 328 264 64 64, 0)
    , ("bottom_walls", Rect 136 264 64 64, 0)
    , ("bottom_walls", Rect 264 264 64 64, 0)
    ]) ++ [AnimationInstance "char_idle_right" (Rect 136 186 64 64) (aiAliveTicks ((animationInfo game)!!0))]

inHouseSokoban :: InHouseSokoban
inHouseSokoban = InHouseSokoban (Sokoban 0 $ loadBoard exampleBoard) [AnimationInfo "char_idle_right" True 0 0]
