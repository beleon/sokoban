module Main where

import           InHouse.Engine
import           InHouseSokoban

main :: IO ()
main = inHouseSokoban >>= playUsingSdl
