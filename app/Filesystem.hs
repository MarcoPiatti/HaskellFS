module Filesystem where

import Directory
import Path

-- Un filesystem es la carpeta raiz, y el path del current (working) directory que tiene la sesion
-- ya que un path mas adelante puede ser absoluto o ser relativo al path actual
data Filesystem = Filesystem 
    { root :: Dir
    , currentPath :: Path
    } deriving (Show, Eq)

initFilesystem :: Filesystem
initFilesystem = Filesystem (emptyDir "") initPath (emptyDir "")