module DirTree where

import FSError
import Data.List

data FSElem = Dir String [FSElem] | File String deriving (Show, Eq)

rootDir :: FSElem
rootDir = Dir "" []

name :: FSElem -> String
name (Dir name _) = name
name (File name) = name

rename :: String -> FSElem -> FSElem
rename name (Dir _ elems) = Dir name elems
rename name (File _) = File name

hasName :: String -> FSElem -> Bool
hasName target elem = target == name elem

addFSElem :: FSElem -> FSElem -> Either FSError FSElem
addFSElem newElem (Dir name elems) = Right $ Dir name (newElem : elems)
addFSElem _ (File _) = Left NotDirectory

deleteFSElem :: FSElem -> FSElem -> Either FSError FSElem
deleteFSElem targetElem (Dir name elems) 
    | targetElem `elem` elems = Right $ Dir name (delete targetElem elems)
    | otherwise = Left DoesNotExist