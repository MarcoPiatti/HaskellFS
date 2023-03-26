module Cursor where

import FSError
import DirTree
import Data.List

-- Informacion sobre una carpeta por encima de una carpeta actual
-- Tiene todos los datos necesarios para reconstruir la carpeta padre
-- a partir de este data y la carpeta actual
data Crumb = Crumb 
    { visitedName :: String
    , visitedLeft :: [FSElem]
    , visitedRight :: [FSElem]
    } deriving (Show, Eq)

-- Una carpeta actual y la lista de carpetas visitadas previamente
type Cursor = (FSElem, [Crumb])

crumb :: FSElem -> FSElem -> Crumb
crumb (Dir name children) child = 
    let (left, elem:right) = break (== child) children
    in Crumb name left right

uncrumb :: FSElem -> Crumb -> FSElem
uncrumb child (Crumb name left right) = Dir name (left ++ [child] ++ right)

cursor :: FSElem -> Cursor
cursor root = (root, [])

rootCursor :: Cursor
rootCursor = cursor rootDir

root :: Cursor -> FSElem
root (current, crumbs) = foldl' uncrumb current crumbs

descendTo :: FSElem -> Cursor -> Either FSError Cursor
descendTo (File _) _ = Left NotDirectory 
descendTo child (current, crumbs) = Right (child, crumb current child : crumbs)

ascend :: Cursor -> Cursor
ascend (current, []) = (current, [])
ascend (current, crumb:visited) = (uncrumb current crumb, visited)

restart :: Cursor -> Cursor
restart cursor = (root cursor, [])
