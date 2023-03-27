module Main where

import DirTree
import Cursor
import Command
import Parser
import CommandWrap
import System.IO

main :: IO ()
main = do
    putStrLn "Welcome to the file system!"
    putStrLn ""
    mainLoop $ cursor $ emptyDir ""

mainLoop :: Cursor -> IO ()
mainLoop cursor = do
    input <- prompt $ pathstring cursor
    let parsedCommand = runParser commandP input
    case parsedCommand of
        Left parseError -> do
            print parseError
            mainLoop cursor
        Right (command, consumedLine) -> do
            let (cursor', resultStr) = wrapCommand command cursor
            showResult resultStr
            mainLoop cursor'

prompt :: String -> IO String
prompt p = putStr (p ++ " >") >> hFlush stdout >> getLine

showResult :: String -> IO ()
showResult "" = return ()
showResult str = putStrLn str