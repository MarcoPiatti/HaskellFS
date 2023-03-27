module DirTree where

import FSError
import Data.List

data FSElem = Dir String [FSElem] | File String deriving (Show, Eq)

emptyDir :: String -> FSElem
emptyDir name = Dir name []

name :: FSElem -> String
name (Dir name _) = name
name (File name) = name

rename :: String -> FSElem -> FSElem
rename name (Dir _ elems) = Dir name elems
rename name (File _) = File name

hasName :: String -> FSElem -> Bool
hasName target elem = target == name elem

addFSElem :: FSElem -> FSElem -> Either FSError FSElem
addFSElem newElem (Dir dirname elems) = case filter (hasName $ name newElem) elems of
    [] -> Right $ Dir dirname (newElem : elems)
    _ -> Left AlreadyExists
addFSElem _ (File _) = Left NotDirectory

addorReplaceFSElem :: FSElem -> FSElem -> Either FSError FSElem
addorReplaceFSElem newElem (Dir dirname elems) = case filter (hasName $ name newElem) elems of
    [] -> Right $ Dir dirname (newElem : elems)
    [elem] -> Right $ Dir dirname (newElem : delete elem elems)
    _ -> Left AlreadyExists

deleteFSElem :: FSElem -> FSElem -> Either FSError FSElem
deleteFSElem targetElem (Dir name elems) 
    | targetElem `elem` elems = Right $ Dir name (delete targetElem elems)
    | otherwise = Left DoesNotExist

findChildByName :: String -> FSElem -> Either FSError FSElem
findChildByName targetName (Dir _ elems) = case filter (hasName targetName) elems of
    [] -> Left DoesNotExist
    [elem] -> Right elem