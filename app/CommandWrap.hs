module CommandWrap where

import Command
import FSError
import DirTree
import Cursor

type CommandF = Cursor -> (Cursor, String)

wrapEffect :: (Cursor -> Either FSError Cursor) -> CommandF
wrapEffect f cursor = case f cursor of
    Left err -> (cursor, show err)
    Right cursor' -> (cursor', "")

wrapQuery :: (Cursor -> Either FSError String) -> CommandF
wrapQuery f cursor = case f cursor of
    Left err -> (cursor, show err)
    Right str -> (cursor, str)

wrapCommand :: Command -> CommandF
wrapCommand (Cd p) = wrapEffect $ cd p
wrapCommand Ls = wrapQuery ls
wrapCommand (Touch p) = wrapEffect $ touch p
wrapCommand (Mkdir p) = wrapEffect $ mkdir p
wrapCommand (Rm p) = wrapEffect $ deleteFSElem p