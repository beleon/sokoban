module InHouseSokoban where

import InHouse.Game
import InHouse.Frame
import InHouse.Resources
import InHouse.Util
import Sokoban
import qualified SDL
import Data.Maybe
import Control.Lens ((^.))

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.List (sortBy, intercalate)

data InHouseSokoban = InHouseSokoban
  { sokoban :: Sokoban
  , animationInfo :: [AnimationInfo]
  , maps :: [String]
  , currentMap :: Int
  }

data Placements = Placements
  { pTargets :: [Location]
  , pBoxes   :: [Location]
  , pBoxesOnTarget   :: [Location]
  , pFloorTiles  :: [Location]
  , pPlayer  :: Location
  , pLeftWalls :: [Location]
  , pRightWalls :: [Location]
  , pBottomWalls :: [Location]
  , pTopWalls :: [Location]
  , pBottomLeftWalls :: [Location]
  , pBottomRightWalls :: [Location]
  , pTopLeftWalls :: [Location]
  , pTopRightWalls :: [Location]
  }

nextDir :: [SDL.EventPayload] -> Maybe Direction
nextDir [] = Nothing
nextDir (x:xs) = case x of
                 (SDL.KeyboardEvent a) -> if SDL.keyboardEventKeyMotion a == SDL.Pressed && not (SDL.keyboardEventRepeat a)
                                          then case SDL.keysymKeycode (SDL.keyboardEventKeysym a) of
                                                 SDL.KeycodeUp    -> Just South
                                                 SDL.KeycodeDown  -> Just North
                                                 SDL.KeycodeLeft  -> Just West
                                                 SDL.KeycodeRight -> Just East
                                                 _                -> r
                                          else r
                 _ -> r
  where r = nextDir xs

instance Game InHouseSokoban where
  titleText _ = "Sokoban"
  update game events = do
    let quit = any (\x -> SDL.keyboardEventKeyMotion x == SDL.Pressed && SDL.keysymKeycode (SDL.keyboardEventKeysym x) == SDL.KeycodeQ) keyevents || SDL.QuitEvent `elem` events
        keyevents = mapMaybe (\x -> case x of {(SDL.KeyboardEvent a) -> Just a; _ -> Nothing}) events
        [ai] = animationInfo game
        nd = nextDir events
        moveResult = fmap (advance (sokoban game)) nd
        w  = maybe False (^. won) moveResult
        ni = if w then currentMap game + 1 else currentMap game
        ng = if w then Sokoban 0 (loadBoard (maps game!!ni)) else maybe (sokoban game) (^. sokoban') moveResult
    now <- fmap fromIntegral SDL.ticks
    let newAlive = aiAliveTicks ai + now - aiLastUpdated ai
    return $ Update quit (game { sokoban = ng
                               , animationInfo = [ai {aiLastUpdated = now, aiAliveTicks = newAlive}]
                               , currentMap = ni
                               })
  frame game =
    let b = ((sokoban game) ^. board)
        p = getPlacements b
        dir = (b ^. player) ^. direction
        dirStr = case dir of
                   North -> "down"
                   South -> "up"
                   East -> "right"
                   West -> "left"
        layers = [ map (\l@(Location x y) -> (l, StillInstance "floor_tiles" (zoomRect (Rect (16 * x) (16 * y) 16 16) scale) 0)) $ pFloorTiles p
                 , map (\l@(Location x y) -> (l, StillInstance "targets" (zoomRect (Rect (16 * x) (16 * y) 16 16) scale) 0)) $ pTargets p
                 , (map (\l@(Location x y) -> (l, StillInstance "boxes" (zoomRect (Rect (16 * x) (16 * y) 16 16) scale) 0)) $ pBoxes p)
                   ++ (map (\l@(Location x y) -> (l, StillInstance "boxes_on_target" (zoomRect (Rect (16 * x) (16 * y) 16 16) scale) 0)) $ pBoxesOnTarget p)
                   ++ (map (\l@(Location x y) -> (l, AnimationInstance ("char_idle_" ++ dirStr) (zoomRect (Rect (16 * x) (16 * y - 3) 16 16) scale) (aiAliveTicks ((animationInfo game)!!0)))) $ [pPlayer p])
                   ++ (map (\l@(Location x y) -> (l, StillInstance "left_walls" (zoomRect (Rect (16 * x) (16 * y) 16 16) scale) 0)) $ pLeftWalls p)
                   ++ (map (\l@(Location x y) -> (l, StillInstance "right_walls" (zoomRect (Rect (16 * x) (16 * y) 16 16) scale) 0)) $ pRightWalls p)
                   ++ (map (\l@(Location x y) -> (l, StillInstance "bottom_walls" (zoomRect (Rect (16 * x) (16 * y) 16 16) scale) 0)) $ pBottomWalls p)
                   ++ (map (\l@(Location x y) -> (l, StillInstance "top_walls" (zoomRect (Rect (16 * x) (16 * y) 16 16) scale) 0)) $ pTopWalls p)
                   ++ (map (\l@(Location x y) -> (l, StillInstance "bottom_left_walls" (zoomRect (Rect (16 * x) (16 * y) 16 16) scale) 0)) $ pBottomLeftWalls p)
                   ++ (map (\l@(Location x y) -> (l, StillInstance "bottom_right_walls" (zoomRect (Rect (16 * x) (16 * y) 16 16) scale) 0)) $ pBottomRightWalls p)
                   ++ (map (\l@(Location x y) -> (l, StillInstance "top_left_walls" (zoomRect (Rect (16 * x) (16 * y) 16 16) scale) 0)) $ pTopLeftWalls p)
                   ++ (map (\l@(Location x y) -> (l, StillInstance "top_right_walls" (zoomRect (Rect (16 * x) (16 * y) 16 16) scale) 0)) $ pTopRightWalls p)
                 ]
        layers' = map (map snd . sortBy (\a b -> renderOrder (fst a) (fst b))) layers
    --in Frame [(map (\(a,b,c) -> StillInstance a b c)
    --[ ("top_left_walls", Rect 72 136 64 64, 0)
    --, ("top_walls", Rect 136 136 64 64, 0)
    --, ("top_walls", Rect 200 136 64 64, 0)
    --, ("top_walls", Rect 264 136 64 64, 0)
    --, ("top_right_walls", Rect 328 136 64 64, 0)
    --, ("left_walls", Rect 72 200 64 64, 0)
    --, ("floor_tiles", Rect 136 200 64 64, 0)
    --, ("floor_tiles", Rect 264 200 64 64, 0)
    --, ("right_walls", Rect 328 200 64 64, 0)
    --, ("boxes", Rect 200 200 64 64, 0)
    --, ("boxes", Rect 200 264 64 64, 0)
    --, ("bottom_left_walls", Rect 72 264 64 64, 0)
    --, ("bottom_right_walls", Rect 328 264 64 64, 0)
    --, ("bottom_walls", Rect 136 264 64 64, 0)
    --, ("bottom_walls", Rect 264 264 64 64, 0)
    --]) ++
    --[ AnimationInstance "char_attack_right" (Rect 136 186 64 84) (aiAliveTicks ((animationInfo game)!!0))
    --, AnimationInstance "char_idle_right" (Rect 172 186 64 64) (aiAliveTicks ((animationInfo game)!!0))]]
    in Frame layers'

scale = 4

getPlacements :: Board -> Placements
getPlacements b =
  let t = b ^. targets
      o = b ^. obstacles
      p = (b ^. player) ^. location
      e = []
      (walls, boxes) = foldl (\(w, b) (l, e) -> case e of
                                                  Wall -> (l:w, b)
                                                  Box ->  (w, l:b)) ([], []) $ M.toList o
      (boxesOnT, boxesOffT) = foldl (\(onT, offT) l -> if elem l t then (l:onT, offT) else (onT, l:offT)) ([], []) boxes
  in Placements
       t--pTargets :: [Location]
       boxesOffT--pBoxes   :: [Location]
       boxesOnT--pBoxesOnTarget   :: [Location]
       e--pFloorTiles  :: [Location]
       p--pPlayer  :: Location
       e--pLeftWalls :: [Location]
       e--pRightWalls :: [Location]
       e--pBottomWalls :: [Location]
       walls--pTopWalls :: [Location]
       e--pBottomLeftWalls :: [Location]
       e--pBottomRightWalls :: [Location]
       e--pTopLeftWalls :: [Location]
       e--pTopRightWalls :: [Location]

renderOrder :: Location -> Location -> Ordering
renderOrder (Location x1 y1) (Location x2 y2) = case compare y1 y2 of
                                                  EQ -> compare x1 x2
                                                  a  -> a

inHouseSokoban :: IO InHouseSokoban
inHouseSokoban = do
  m <- fmap (map (intercalate "\n") . split [] . lines) $ readFile "maps/classic.txt"
  return $ InHouseSokoban (Sokoban 0 (loadBoard (head m))) [AnimationInfo "char_idle_right" True 0 0] m 0
