module FSError where

data FSError 
    = NotDirectory 
    | NotFile 
    | AlreadyExists 
    | DoesNotExist 
    deriving (Show, Eq)
