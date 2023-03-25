module Directory where

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

renameDir :: String -> Dir -> Dir
renameDir newName dir = dir { dirname = newName }

addFile :: File -> Dir -> Dir
addFile newFile dir = dir { files = newFile : files dir }

removeFile :: File -> Dir -> Dir
removeFile targetFile dir = dir { files = filter (/= targetFile) . files $ dir }

addSubdir :: Dir -> Dir -> Dir
addSubdir newDir dir = dir { subdirs = newDir : subdirs dir }

removeSubdir :: Dir -> Dir -> Dir
removeSubdir targetDir dir = dir { subdirs = filter (/= targetDir) . subdirs $ dir }