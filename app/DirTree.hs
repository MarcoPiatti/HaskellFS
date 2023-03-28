module DirTree where

import FSError
import Data.List

data FSElem = Dir String [FSElem] | File String deriving (Show, Eq)

emptyDir :: String -> FSElem
emptyDir name = Dir name []

name :: FSElem -> String
name (Dir name _) = name
name (File name) = name

isDir :: FSElem -> Bool
isDir (Dir _ _) = True
isDir (File _) = False

prettyPrint :: FSElem -> String
prettyPrint (Dir name _) = name ++ "/"
prettyPrint (File name) = name

rename :: String -> FSElem -> FSElem
rename name (Dir _ elems) = Dir name elems
rename name (File _) = File name

hasName :: String -> FSElem -> Bool
hasName target elem = target == name elem

addChild :: FSElem -> FSElem -> Either FSError FSElem
addChild newElem (Dir dirname elems) = case filter (hasName $ name newElem) elems of
    [] -> Right $ Dir dirname (newElem : elems)
    _ -> Left AlreadyExists
addChild _ (File _) = Left NotDirectory

addOrReplaceChild :: FSElem -> FSElem -> Either FSError FSElem
addOrReplaceChild newElem (Dir dirname elems) = case filter (hasName $ name newElem) elems of
    [] -> Right $ Dir dirname (newElem : elems)
    [elem] -> Right $ Dir dirname (newElem : delete elem elems)
    _ -> Left AlreadyExists

-- asume que el elemento ya es hijo
-- asume que nunca se lo llama desde un archivo
deleteChild :: FSElem -> FSElem -> FSElem
deleteChild targetElem (Dir name elems) = Dir name (delete targetElem elems)

findChildByName :: String -> FSElem -> Either FSError FSElem
findChildByName targetName (Dir _ elems) = case filter (hasName targetName) elems of
    [] -> Left DoesNotExist
    [elem] -> Right elem