module Main where
import Command
import Lib

data Position = Position 
  { horizontal :: Integer
  , height :: Integer
  , aim :: Integer
  } deriving Show

move :: [Command Integer] -> Position -> Position
move [] p = p 
move ((Forward x):xs) p = move xs (p { horizontal = (horizontal p) + x, height = (height p + (aim p * x)) })
move ((Down x):xs) p = move xs (p { aim = (aim p) + x })
move ((Up x):xs) p = move xs (p { aim = (aim p) - x })

initPosition = Position 0 0 0

main :: IO ()
main = putStrLn $ show $ move moves initPosition
