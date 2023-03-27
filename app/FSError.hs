{-# LANGUAGE FlexibleInstances #-}

module FSError where

import Control.Applicative

data FSError 
    = NotDirectory 
    | NotFile 
    | AlreadyExists 
    | DoesNotExist 
    | CantRemoveRoot
    | Empty
    deriving (Show, Eq)

instance Alternative (Either FSError) where
    empty = Left Empty
    Left _ <|> e2 = e2
    Right a <|> _ = Right a