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
import Data.List (sortBy, intercalate, sortOn)

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
  , pDarkFloorTiles  :: [Location]
  , pPlayer  :: Location
  , pLeftWalls :: [Location]
  , pRightWalls :: [Location]
  , pBottomWalls :: [Location]
  , pTopWalls :: [Location]
  , pBottomLeftWalls :: [Location]
  , pBottomRightWalls :: [Location]
  , pTopLeftWalls :: [Location]
  , pTopRightWalls :: [Location]
  , pCenterWalls :: [Location]
  , pLeftOutsideWalls :: [Location]
  , pRightOutsideWalls :: [Location]
  }

data LogicalEvents = LogicalEvents
  { resetLevel :: Bool
  }

getLogicalEvents :: [SDL.KeyboardEventData] -> LogicalEvents
getLogicalEvents e =
  LogicalEvents $ any (\x -> SDL.keyboardEventKeyMotion x == SDL.Pressed && SDL.keysymKeycode (SDL.keyboardEventKeysym x) == SDL.KeycodeR) e
  

nextDir :: [SDL.EventPayload] -> Maybe Direction
nextDir [] = Nothing
nextDir (x:xs) = case x of
                 (SDL.KeyboardEvent a) -> if SDL.keyboardEventKeyMotion a == SDL.Pressed && not (SDL.keyboardEventRepeat a)
                                          then case SDL.keysymKeycode (SDL.keyboardEventKeysym a) of
                                                 SDL.KeycodeUp    -> Just South
                                                 SDL.KeycodeDown  -> Just North
                                                 SDL.KeycodeLeft  -> Just West
                                                 SDL.KeycodeRight -> Just East
                                                 SDL.KeycodeK     -> Just South
                                                 SDL.KeycodeJ     -> Just North
                                                 SDL.KeycodeH     -> Just West
                                                 SDL.KeycodeL     -> Just East
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
        ng = case undefined of
               _ | resetLevel le || w -> Sokoban 0 (loadBoard (maps game!!ni))
                 | True -> maybe (sokoban game) (^. sokoban') moveResult
        le = getLogicalEvents keyevents
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
                 , map (\l@(Location x y) -> (l, StillInstance "dark_floor_tiles" (zoomRect (Rect (16 * x) (16 * y) 16 16) scale) 0)) $ pDarkFloorTiles p
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
                   ++ (map (\l@(Location x y) -> (l, StillInstance "center_walls" (zoomRect (Rect (16 * x) (16 * y) 16 16) scale) 0)) $ pCenterWalls p)
                   ++ (map (\l@(Location x y) -> (l, StillInstance "left_outside_walls" (zoomRect (Rect (16 * x) (16 * y) 16 16) scale) 0)) $ pLeftOutsideWalls p)
                   ++ (map (\l@(Location x y) -> (l, StillInstance "right_outside_walls" (zoomRect (Rect (16 * x) (16 * y) 16 16) scale) 0)) $ pRightOutsideWalls p)
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

outside (Location u v) walls = not (any (\(Location x y) -> u < x && y == v) walls) || not (any (\(Location x y) -> u > x && y == v) walls) || not (any (\(Location x y) -> v < y && x == u) walls) || not (any (\(Location x y) -> v > y && x == u) walls)

idWalls walls =
  let compX f (Location x1 y1) (Location x2 y2) = y1 == y2 && f x1 x2
      compY f (Location x1 y1) (Location x2 y2) = x1 == x2 && f y1 y2
      outside' = flip outside walls
      m = map (\l@(Location x y)
                 -> let left = Location (x - 1) y
                        right = Location (x + 1) y
                        up = Location x (y - 1)
                        down = Location x (y + 1)
                        downLeft = Location (x - 1) (y + 1)
                        downRight = Location (x + 1) (y + 1)
                        nw l = not $ elem l walls
                    in (l, case undefined of _ | nw left && nw up && outside' left -> 4
                                               | nw left && nw down && outside' left -> 6
                                               | nw right && nw up && outside' right -> 5
                                               | nw right && nw down && outside' right -> 7
                                               | nw left && outside' left -> 0
                                               | nw right && outside' right -> 1
                                               | nw down && outside' down -> 3
                                               | not (nw down) && not (nw left) && outside' downLeft -> 9
                                               | not (nw down) && not (nw right) && outside' downRight -> 10
                                               | nw down && outside' down -> 3
                                               | not $ nw down -> 8
                                               | True -> 2)) walls
  in (\[a, b, c, d, e, f, g, h, i, j, k] -> (a, b, c, d, e, f, g, h, i, j, k)) $ map (map fst) $ (\l -> l ++ (take (11 - length l) $ repeat [])) $ reverse $ snd $ foldl (\(c, l) g@((_, i):_) -> (c+1, (g:(take (i - c) $ repeat []))++l)) (0, []) $ groupOn snd $ sortOn snd m

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
      (xm, ym) = foldl (\(a, b) (Location x y) -> (max a x, max b y)) (0, 0) walls
      outside' = flip outside walls
      floor = [Location x y | x <- [0..xm], y <- [0..ym], not $ outside' $ Location x y]
      darkFloor = [Location x y | x <- [0..xm], y <- [0..ym], outside' $ Location x y]
      (lw, rw, tw, bw, tlw, trw, blw, brw, cw, low, row) = idWalls walls
  in Placements
       t--pTargets :: [Location]
       boxesOffT--pBoxes   :: [Location]
       boxesOnT--pBoxesOnTarget   :: [Location]
       floor--pFloorTiles  :: [Location]
       darkFloor--pFloorTiles  :: [Location]
       p--pPlayer  :: Location
       lw--pLeftWalls :: [Location]
       rw--pRightWalls :: [Location]
       bw--pBottomWalls :: [Location]
       tw--pTopWalls :: [Location]
       blw--pBottomLeftWalls :: [Location]
       brw--pBottomRightWalls :: [Location]
       tlw--pTopLeftWalls :: [Location]
       trw--pTopRightWalls :: [Location]
       cw--pCenterWalls :: [Location]
       low--pLeftOutisdeWalls :: [Location]
       row--pRightOutsideWalls :: [Location]

renderOrder :: Location -> Location -> Ordering
renderOrder (Location x1 y1) (Location x2 y2) = case compare y1 y2 of
                                                  EQ -> compare x1 x2
                                                  a  -> a

inHouseSokoban :: IO InHouseSokoban
inHouseSokoban = do
  m <- fmap (map (intercalate "\n") . split [] . lines) $ readFile "maps/classic.txt"
  return $ InHouseSokoban (Sokoban 0 (loadBoard (head m))) [AnimationInfo "char_idle_right" True 0 0] m 0
