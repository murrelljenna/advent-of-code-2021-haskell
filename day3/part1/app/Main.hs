module Main where

import Lib
import Data.List
import Data.Function (on)
import Input

getAt :: Int -> String -> String
getAt i ss = [ss !! i]

acc :: Int -> String -> String -> String
acc i a s = a ++ (getAt i s)

column :: [String] -> Int -> String
column ss i = foldl (acc i) "" ss

mostCommonBit :: String -> Char
mostCommonBit s = head $ maximumBy (compare `on` length) $ groupBy (==) $ sort s

leastCommonBit :: String -> Char
leastCommonBit s = head $ minimumBy (compare `on` length) $ groupBy (==) $ sort s

main :: IO ()
main = do 
  let cl = length $ input !! 0
  let inputColumnAt = column input
  let transposed = map inputColumnAt [0..cl-1]
  let gamma = map (mostCommonBit.inputColumnAt) [0..cl-1]
  putStrLn $ show gamma
  let epsilon = map (leastCommonBit.inputColumnAt) [0..cl-1]
  putStrLn $ show epsilon
