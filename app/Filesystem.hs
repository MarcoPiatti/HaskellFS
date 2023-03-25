module Filesystem where

-- Una carpeta con nombre, otras carpetas, y archivos
data Dir = Dir 
    { dirname :: String
    , subdirs :: [Dir]
    , files :: [File] 
    } deriving (Show, Eq)

-- Un archivo con nombre
data File = File 
    { filename :: String
    } deriving (Show, Eq)

-- Informacion sobre una carpeta por encima de una carpeta actual
-- Tiene todos los datos necesarios para reconstruir la carpeta padre
-- a partir de este data y la carpeta actual
data Visited = Visited 
    { visitedName :: String
    , visitedFiles :: [File]
    , visitedLeftSubdirs :: [Dir]
    , visitedRightSubdirs :: [Dir]
    } deriving (Show, Eq)

-- Una carpeta actual y la lista de carpetas visitadas previamente
type Path = (Dir, [Visited])

-- Un filesystem es la carpeta raiz, y el path del current (working) directory que tiene la sesion
-- ya que un path mas adelante puede ser absoluto o ser relativo al path actual
data Filesystem = Filesystem 
    { root :: Dir
    , currentPath :: Path
    } deriving (Show, Eq)

-- Quizas separar en modulos para dir y file por un lado, otro para path, y otro para fs

updateDir :: (Dir -> Dir) -> Dir -> Dir -> Dir
updateDir updater targetDir root
    | root == targetDir = updater root
    | otherwise = root { subdirs = map (updateDir updater targetDir) . subdirs $ root }

addFile :: File -> Dir -> Dir -> Dir
addFile newFile = updateDir (\d -> d { files = newFile : files d })

removeFile :: File -> Dir -> Dir -> Dir
removeFile targetFile = updateDir (\d -> d { files = filter (/= targetFile) . files $ d })

addSubdir :: Dir -> Dir -> Dir -> Dir
addSubdir newDir = updateDir (\d -> d { subdirs = newDir : subdirs d })

removeSubdir :: Dir -> Dir -> Dir -> Dir
removeSubdir targetDir = updateDir (\d -> d { subdirs = filter (/= targetDir) . subdirs $ d })

