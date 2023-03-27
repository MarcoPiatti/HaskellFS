{-# LANGUAGE TupleSections #-}

module Command where

import FSError
import DirTree
import Cursor

{- 
cd: Cambia el CWD segun un path
asumiendo un CWD /dir1/dir2
un path puede ser absoluto como /dir1/dir2/dir3
o relativo como ./dir3 o ../dir2b

una forma de resolver la traducción de un path a un traversal del cursor es
interpretar cada string separada entre '/' como un comando de recorrido.
. se resuelve a la funcion id (no cambia)
.. se resuelve a la funcion ascend
nombre se resuelve a la funcion descendTo, del cual el nombre debe resolverse a uno valido

la resolución de un pathstring a la secuencia apropiada la hacemos en otro modulo.

luego a partir de esta lista de direcciones es aplicarlas en secuencia sobre el cursor y retornar el resultado.

cd es el unico comando que no hace modificaciones sobre el fs, solo sobre el cursor en si.
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