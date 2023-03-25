module Directory where

import Data.List

-- Un archivo con nombre
data File = File 
    { filename :: String
    } deriving (Show, Eq)

-- Una carpeta con nombre, otras carpetas, y archivos
data Dir = Dir 
    { dirname :: String
    , files :: [File] 
    , subdirs :: [Dir]
    } deriving (Show, Eq)

emptyDir :: String -> Dir
emptyDir name = Dir name [] []

renameFile :: String -> File -> File
renameFile newName file = file { filename = newName }

hasFilename :: String -> File -> Bool
hasFilename name file = name == filename file

renameDir :: String -> Dir -> Dir
renameDir newName dir = dir { dirname = newName }

hasSubdirname :: String -> Dir -> Bool
hasSubdirname name dir = name == dirname dir

addFile :: File -> Dir -> Dir
addFile newFile dir = dir { files = newFile : files dir }

removeFile :: File -> Dir -> Dir
removeFile targetFile dir = dir { files = filter (/= targetFile) . files $ dir }

addSubdir :: Dir -> Dir -> Dir
addSubdir newDir dir = dir { subdirs = newDir : subdirs dir }

removeSubdir :: Dir -> Dir -> Dir
removeSubdir targetDir dir = dir { subdirs = filter (/= targetDir) . subdirs $ dir }

resolveSubdir :: String -> Dir -> Maybe Dir
resolveSubdir subdirName dir = find ((subdirName ==) . dirname) . subdirs $ dir

containsFilename :: String -> Dir -> Bool
containsFilename name dir = any (hasFilename name) . files $ dir

containsSubdirname :: String -> Dir -> Bool
containsSubdirname name dir = any (hasSubdirname name) . subdirs $ dir
