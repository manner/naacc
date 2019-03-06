module Lib
    ( lexing
    , tokenToString
    ) where

import Data.Char
import Text.Show.Functions

data Token = OpenBrace 
            | CloseBrace
            | OpenParen 
            | CloseParen
            | Semicolon
            | IntKeyword
            | ReturnKeyword
            | Id Char
            | IntValue Int
            deriving (Show)  

lexing :: String -> [Token]
lexing [] = []
lexing (x:xs) 
    | x == '{' = OpenBrace:(lexing xs)
    | x == '}' = CloseBrace:(lexing xs)
    | x == '(' = OpenParen:(lexing xs)
    | x == ')' = CloseParen:(lexing xs)
    | x == ';' = Semicolon:(lexing xs)
    | isDigit x = (IntValue $ digitToInt x):(lexing xs)
    | isAlpha x = (Id x):(lexing xs)
    | otherwise = lexing xs
    
tokenToString :: Token -> String
tokenToString token = case token of
    OpenBrace   -> "{"
    CloseBrace  -> "}"
    OpenParen   -> "("
    CloseParen  -> ")"
    Semicolon   -> ";"
    IntValue x  -> show x
    Id x        -> [x]