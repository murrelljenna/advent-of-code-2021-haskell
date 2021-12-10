module Main where

import Lib
import System.IO
import Data.Char(toUpper)

main :: IO ()
main = do
  inh <- openFile "input" ReadMode
  count <- compareLine inh maxBound
  putStrLn $ show count
  hClose inh

compareLine :: Handle -> Int -> IO Int
compareLine inh last = do
  ineof <- hIsEOF inh
  if ineof
    then return 0
    else (do 
      inpStr <- hGetLine inh
      let inpInt = read inpStr :: Int
      count <- compareLine inh inpInt
      if inpInt > last
        then return $ 1 + count
        else return count
      )
