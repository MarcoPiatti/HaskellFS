{-# LANGUAGE TupleSections #-}

module Command where

import FSError
import DirTree
import Cursor
import Data.Either (partitionEithers)

{-
todavia no se bien como encarar la conversion del Data Command a la funcion que a partir
de un cursor genera el nuevo resultado.

me huele que voy a tener que wrappear todo en un IO o algo para que los comandos sean homogeneos entre si.
porque todos generan efecto en el cursor excepto ls que solo printea los contenidos.

quizas haya algun tipo de wrappeo que printea y devuelve el nuevo cursor. veremos
-}
data Command = Cd [Direction] | Ls | Touch [Direction] | Mkdir [Direction] deriving (Show, Eq)

cd :: [Direction] -> Cursor -> Either FSError Cursor
cd = traverseFs

ls :: Cursor -> Either FSError [FSElem]
ls (Dir _ children, _) = Right children
ls (File _, _) = Left NotDirectory

dropLast :: [a] -> [a]
dropLast [] = []
dropLast [x] = []
dropLast (x:xs) = x : dropLast xs

insertFSElem :: (String -> FSElem) -> (FSElem -> FSElem -> Either FSError FSElem) -> [Direction] -> Cursor -> Either FSError Cursor
insertFSElem constructor policy directions = insertFSElem' policy (dropLast directions) (constructor . directionToString . last $ directions)

insertFSElem' :: (FSElem -> FSElem -> Either FSError FSElem) -> [Direction] -> FSElem -> Cursor -> Either FSError Cursor
insertFSElem' policy path newElem cursor = do
    (dir, crumbs) <- traverseFs path cursor
    (,crumbs) <$> policy newElem dir

touch :: [Direction] -> Cursor -> Either FSError Cursor
touch = insertFSElem File addorReplaceFSElem

mkdir :: [Direction] -> Cursor -> Either FSError Cursor
mkdir = insertFSElem emptyDir addFSElem