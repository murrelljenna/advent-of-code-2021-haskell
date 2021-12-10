module Main where

import Lib
import System.IO
import Data.Char(toUpper)
import Prelude

main :: IO ()
main = do
  inh <- openFile "input" ReadMode
  count <- compareLine inh [maxBound]
  putStrLn $ show count
  hClose inh

getLines :: Handle -> Int -> IO [Int]
getLines hdl 0 = return []
getLines hdl x = do
  ineof <- hIsEOF hdl
  if ineof
    then return [0]
    else (do
      lStr <- hGetLine hdl
      let lInt = read lStr :: Int
      rlInt <- getLines hdl (x-1)
      return $ [lInt] ++ rlInt
    )
  

compareLine :: Handle -> [Int] -> IO Int
compareLine inh last = do
  ineof <- hIsEOF inh
  if ineof
    then return 0
    else (do 
      fLn <- getLines inh 1
      pos <- hGetPosn inh
      inp <- getLines inh 2
      hSetPosn pos
      count <- compareLine inh (fLn ++ inp)
      if (sum (fLn ++ inp)) > (sum last)
        then return $ 1 + count
        else return count
      )
