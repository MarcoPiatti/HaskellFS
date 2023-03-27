{-# LANGUAGE TupleSections #-}

module Command where

import FSError
import DirTree
import Cursor
import Data.Either (partitionEithers)
import Control.Applicative ( Alternative((<|>)) )
import Control.Monad ( (>=>) )
import DirTree (deleteChild)
import Data.List (intercalate)

{-
todavia no se bien como encarar la conversion del Data Command a la funcion que a partir
de un cursor genera el nuevo resultado.

me huele que voy a tener que wrappear todo en un IO o algo para que los comandos sean homogeneos entre si.
porque todos generan efecto en el cursor excepto ls que solo printea los contenidos.

quizas haya algun tipo de wrappeo que printea y devuelve el nuevo cursor. veremos
-}
data Command = Cd [Direction] | Ls | Touch [Direction] | Mkdir [Direction] | Rm [Direction] deriving (Show, Eq)

dropLast :: [a] -> [a]
dropLast [] = []
dropLast [x] = []
dropLast (x:xs) = x : dropLast xs

cd :: [Direction] -> Cursor -> Either FSError Cursor
cd p = traverseFs p >=> cd' 

cd' :: Cursor -> Either FSError Cursor
cd' (File _, _) = Left NotDirectory
cd' cursor = Right cursor

ls :: Cursor -> Either FSError String
ls (Dir _ children, _) = Right $ intercalate "\n" . map name $ children
ls (File _, _) = Left NotDirectory

insertFSElem :: (String -> FSElem) -> (FSElem -> FSElem -> Either FSError FSElem) -> [Direction] -> Cursor -> Either FSError Cursor
insertFSElem constructor policy directions = insertFSElem' policy (dropLast directions) (constructor . directionToString . last $ directions)

insertFSElem' :: (FSElem -> FSElem -> Either FSError FSElem) -> [Direction] -> FSElem -> Cursor -> Either FSError Cursor
insertFSElem' policy path newElem cursor = do
    (dir, crumbs) <- traverseFs path cursor
    (,crumbs) <$> policy newElem dir

touch :: [Direction] -> Cursor -> Either FSError Cursor
touch p cursor =  insertFSElem File addOrReplaceChild p cursor >>= traverseFs (path cursor)

mkdir :: [Direction] -> Cursor -> Either FSError Cursor
mkdir p cursor = insertFSElem emptyDir addChild p cursor >>= traverseFs (path cursor)

takeFSElem :: [Direction] -> Cursor -> Either FSError FSElem
takeFSElem p cursor = do
    (elem, _) <- traverseFs p cursor
    return elem

deleteFSElem :: [Direction] -> Cursor -> Either FSError Cursor
deleteFSElem p cursor = do
    cursor' <- traverseFs p cursor
    cursor'' <- deleteFSElem' cursor'
    traverseFs (path cursor) cursor'' <|> Right cursor''

deleteFSElem' :: Cursor -> Either FSError Cursor
deleteFSElem' (_, []) = Left CantRemoveRoot
deleteFSElem' (elem, crumbs) = 
    let (parent, crumbs') = ascend (elem, crumbs)
    in Right (deleteChild elem parent, crumbs')

rm :: [Direction] -> Cursor -> Either FSError Cursor
rm = deleteFSElem