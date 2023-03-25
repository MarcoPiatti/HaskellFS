module Command where

import Path
import Directory

import Control.Monad

-- Un comando procesa un path con chance de errores
type Command = Path -> Maybe Path

validate :: (Path -> Bool) -> Path -> Maybe Path
validate checker path
    | checker path = Just path
    | otherwise = Nothing

-- Cambia el path por otro path
cd :: [Command] -> Command
cd = foldr1 (>=>)

touch :: String -> Command
touch name = (fmap $ updatePathDir $ addFile $ File name) . validate (not . containsFilename name . fst)

mkdir :: String -> Command
mkdir name = (fmap $ updatePathDir $ addSubdir $ emptyDir name) . validate (not . containsSubdirname name . fst)
