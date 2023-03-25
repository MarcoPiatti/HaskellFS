module Path where

import Directory

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

leftDirs :: Dir -> [Dir] -> [Dir]
leftDirs dir subdirs = takeWhile (/= dir) subdirs

rightDirs :: Dir -> [Dir] -> [Dir]
rightDirs dir subdirs = tail . dropWhile (/= dir) $ subdirs

visitedFrom :: Dir -> Dir -> Visited
visitedFrom parent dir = Visited (dirname parent) (files parent) (leftDirs dir . subdirs $ parent) (rightDirs dir . subdirs $ parent) 

dirFrom :: Dir -> Visited -> Dir
dirFrom dir visited = Dir (visitedName visited) (visitedFiles visited) (visitedLeftSubdirs visited ++ [dir] ++ visitedRightSubdirs visited)

initPath :: Dir -> Path
initPath dir = (dir, [])

recreateRoot :: Path -> Dir
recreateRoot (dir, visited) = foldl dirFrom dir visited

visitSubdir :: Dir -> Path -> Path
visitSubdir subdir (current, visited) = (subdir, visitedFrom current subdir : visited)

visitParent :: Path -> Path
visitParent (current, parentVisit:visited) = (dirFrom current parentVisit, visited)

visitRoot :: Path -> Path
visitRoot path = (recreateRoot path, [])

updatePathDir :: (Dir -> Dir) -> Path -> Path
updatePathDir updater (dir, visited) = (updater dir, visited)

visitSubdirByName :: String -> Path -> Maybe Path
visitSubdirByName name path = flip visitSubdir path <$> resolveSubdir name (fst path)