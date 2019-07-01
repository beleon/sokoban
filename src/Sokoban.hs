{-# LANGUAGE TemplateHaskell #-}
module Sokoban where

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Control.Lens hiding (element)
import Control.Lens.TH
import Data.Maybe
import Data.List
import Control.Arrow ((&&&))

type EntityMap = Map Location Entity

data Sokoban = Sokoban
    { _moveCount :: Integer
    , _board     :: Board
    } deriving Show

data MoveResult = MoveResult
    { _updateList :: [Location]
    , _sucess     :: Bool
    , _won        :: Bool
    , _sokoban'      :: Sokoban
    } deriving Show

data Entity = Box | Wall deriving (Show, Eq)

data Location = Location
    { _x :: Int
    , _y :: Int
    } deriving (Eq, Ord, Show)

data Player = Player
    { _location  :: Location
    , _direction :: Direction
    } deriving Show

data Direction = North | East | South | West deriving (Eq, Show)
data MoveType = NormalMove | BoxMove | NoMove deriving (Eq, Show)

data Board = Board
    { _obstacles  :: EntityMap
    , _targets    :: [Location]
    , _player     :: Player
    } deriving Show

data BoardBuilder = BoardBuilder
    { _knownObstacles :: [(Location, Entity)]
    , _knownTargets   :: [Location]
    , _playerLocation :: Maybe Location
    } deriving Show

exampleBoard :: String
--exampleBoard = "*###*\n#@$.#\n*###*"
exampleBoard = "    #####\n\
               \    #   #\n\
               \    #$  #\n\
               \  ###  $###\n\
               \  #  $  $ #\n\
               \### # ### #     ######\n\
               \#   # ### #######  ..#\n\
               \# $  $             ..#\n\
               \##### #### #@####  ..#\n\
               \    #      ###  ######\n\
               \    ########"

makeLenses ''Sokoban
makeLenses ''MoveResult
makeLenses ''Entity
makeLenses ''Location
makeLenses ''Player
makeLenses ''Direction
makeLenses ''Board
makeLenses ''BoardBuilder

loadBoard :: String -> Board
loadBoard s = toBoard $ constructBoardBuilder (BoardBuilder [] [] Nothing) s $ Location 0 0

toBoard :: BoardBuilder -> Board
toBoard bb =
    let o = M.fromList $ bb ^. knownObstacles
        t = bb ^. knownTargets
        l = maybe (error "Player position was not set") id $ bb ^. playerLocation
    in Board o t (Player l South)

constructBoardBuilder :: BoardBuilder -> String -> Location -> BoardBuilder
constructBoardBuilder bb []        _ = bb
constructBoardBuilder bb ('\n':es) l = constructBoardBuilder bb es $ Location 0 $ inc $ l ^. y
constructBoardBuilder bb (e:es)    l =
    let setPlayerLocation = (playerLocation .~ Just l)
        addTarget         = knownTargets %~ (l:)
        addObstacle ot    = knownObstacles %~ ((l, ot):)
        fns               = case e of
                             '#' -> [addObstacle Wall]
                             '@' -> [setPlayerLocation]
                             '+' -> [setPlayerLocation, addTarget]
                             '$' -> [addObstacle Box]
                             '*' -> [addObstacle Box, addTarget]
                             '.' -> [addTarget]
                             ' ' -> []
                             e   -> error $ "Invalid map character: " ++ [e]
        bb'               = foldl (flip ($)) bb fns
    in constructBoardBuilder bb' es $ x %~ inc $ l

inc :: Num a => a -> a
inc a = a + 1

dec :: Num a => a -> a
dec a = a - 1

move :: Location -> Direction -> Location
move l North = y %~ inc $ l
move l East  = x %~ inc $ l
move l South = y %~ dec $ l
move l West  = x %~ dec $ l

-- unsafe update (no check if entity exists at Location)
updateEntityLocation :: Board -> Location -> Location -> Board
updateEntityLocation b l1 l2 = obstacles %~ (uncurry (M.insert l2) . ((M.! l1) &&& (M.delete l1))) $  b

-- unsafe update (no check if entity exists at Location)
updatePlayerLocation :: Board -> Location -> Board
updatePlayerLocation b l = player %~ (location .~ l) $  b

updatePlayerDirection :: Board -> Direction -> Board
updatePlayerDirection b d = player %~ (direction .~ d) $ b

getObstacle :: Board -> Location -> Maybe Entity
getObstacle b l = (b ^. obstacles) M.!? l

hasWon :: Board -> Bool
hasWon b =
    let t = b ^. targets
        e = b ^. obstacles
    in all (Just Box==) $ map (e M.!?) t

advance :: Sokoban -> Direction -> MoveResult
advance s d =
    let b        = s ^. board
        p        = b ^. player
        l        = p ^. location
        l'       = move l d
        l''      = move l' d
        o        = getObstacle b l'                -- obstacle at Location player wants to move
        blk      = not $ isNothing $ getObstacle b l''   -- is Location where box would move blocked?
        b'       = updatePlayerDirection b d       -- board after player direction is updated
        b''      = updatePlayerLocation b' l'      -- board after player move
        b'''     = updateEntityLocation b'' l' l'' -- board after player move and box move
        mt       = case o of                       -- which type of move is performed?
                     Just Wall -> NoMove
                     Just Box  -> if blk then NoMove else BoxMove
                     Nothing   -> NormalMove
        -- how does the new board look like and which Locations changed due to the move?
        (nb, ul) = case mt of
                     NoMove     -> (,) b' []
                     BoxMove    -> (,) b''' [l, l', l'']
                     NormalMove -> (,) b'' [l, l']
        st       = mt /= NoMove
        s'       = board .~ nb $ moveCount %~ inc $ s
        w        = hasWon nb
    in MoveResult ul st w s'

playGame :: [Direction] -> Sokoban -> [MoveResult]
playGame es s =
    let b = s ^. board
        w = hasWon b
    in if w then [] else takeUntil (^. won) $ playGame' es s

playGame' :: [Direction] -> Sokoban -> [MoveResult]
playGame' []     _ = []
playGame' (e:es) s = let mr = advance s e in mr:playGame' es (mr ^. sokoban')

printGame :: Sokoban -> String
printGame s =
    let b       = s ^. board
        e       = b ^. obstacles
        t       = b ^. targets
        l       = M.keys e ++ t
        xLen    = maximum $ map (^. x) l
        yLen    = maximum $ map (^. y) l
        nextL l = if l ^. x < xLen - 1 then x %~ inc $ l else Location 0 (1 + (l ^. y))
        isT l   = l `elem` t
        isP l   = l == (b ^. player) ^. location
        ib      = [[ if ip
                     then if it then '+' else '@'
                     else case n of
                            Nothing     -> if it then '.' else ' '
                            (Just Wall) -> '#'
                            (Just Box)  -> if it then '*' else '$'
                   | a <- [0..xLen]
                   , let l = Location a b
                   , let it = isT l
                   , let ip = isP l
                   , let n = e M.!? l]
                  | b <- [0..yLen]]
    in intercalate "\n" ib ++ "\n"

parseDirection :: String -> Direction
parseDirection "s" = North
parseDirection "d" = East
parseDirection "w" = South
parseDirection "a" = West
parseDirection s   = error $ "Invalid direction: \"" ++ s ++ "\""

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil f []     = []
takeUntil f (a:as) = if f a then [a] else a:takeUntil f as

playOnTerminal :: String -> String
playOnTerminal inp =
    let initS = Sokoban 0 $ loadBoard exampleBoard
        res   = map (^. sokoban') $ playGame (map parseDirection $ lines inp) initS
    in printGame initS ++ (concat $ map printGame res)
