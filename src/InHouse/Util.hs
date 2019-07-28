module InHouse.Util where

import Data.List
import Data.Maybe
import Data.Char

type Color = (Int, Int, Int)

parseHex :: String -> Int
parseHex [] = error "Nothing to parse"
parseHex s  =
  let parseHex' [x]    = parseHexCh x
      parseHex' (x:xs) = parseHexCh x + 16 * parseHex' xs
      parseHexCh ch    = fromJust $ elemIndex ch "0123456789abcdef"
  in parseHex' $ map toLower $ reverse s

parseColor :: String -> (Int, Int, Int)
parseColor (ca1:ca2:cb1:cb2:cc1:cc2:[]) = (parseHex [ca1, ca2], parseHex [cb1, cb2], parseHex [cc1, cc2])
parseColor s                            = error $ "Invalid string length for color. Expected 6 characters but got " ++ show (length s) ++ "."

-- Rect x y x_width y_width
data Rectangle = Rect Int Int Int Int

data V2 = V2 Int Int

instance Num V2 where
  negate (V2 a b) = V2 (negate a) (negate b)
  abs (V2 a b) = V2 (abs a) (abs b)
  signum (V2 a b) = V2 (signum a) (signum b)
  (V2 a b) * (V2 x y) = V2 (a * x) (b * y)
  (V2 a b) + (V2 x y) = V2 (a + x) (b + y)
  fromInteger a = V2 (fromInteger a) 0

fromTuple :: (Int, Int) -> V2
fromTuple (x, y) = V2 x y

moveRect :: Rectangle -> V2 -> Rectangle
moveRect (Rect x y w h) (V2 mx my) = Rect (x + mx) (y + my) w h

zoomRect :: Rectangle -> Int -> Rectangle
zoomRect (Rect x y w h) s = Rect (s * x) (s * y) (s * w) (s * h)


dim :: Rectangle -> V2
dim (Rect _ _ w h) = V2 w h

loc :: Rectangle -> V2
loc (Rect x y _ _) = V2 x y

split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = []
split s l  =
  let (a, b) = span (/=s) l
  in case b of
       []     -> [a]
       (x:xs) -> a:split s xs

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn _ [] = []
groupOn _ [x] = [[x]]
groupOn f (x1:x2:xs) = if f x1 == f x2
                       then let (r:rs) = groupOn f (x2:xs) in (x1:r):rs
                       else [x1]:groupOn f (x2:xs)
