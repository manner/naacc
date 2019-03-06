module Main where

import Lib
import System.IO  
import Data.List


main :: IO ()
main = do
    handle <- openFile "return_2.c" ReadMode
    contents <- hGetContents handle 
    let tokens = lexing contents
        stringTokens = map tokenToString tokens 
    putStr $ unwords stringTokens
    hClose handle

