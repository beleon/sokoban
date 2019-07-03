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
