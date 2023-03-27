{-# LANGUAGE FlexibleInstances #-}

module Parser where


import Cursor
import Command
import Control.Applicative
import Data.Char

data ParseError = ParseError deriving (Show, Eq)

newtype Parser a = Parser 
    { runParser :: String -> Either ParseError (a, String)
    }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \s -> do
        (a', s') <- p s
        return (f a', s')

instance Applicative Parser where
    pure a = Parser $ \s -> Right (a, s)
    (Parser p1) <*> (Parser p2) = Parser $ \s -> do
        (f, s') <- p1 s
        (a, s'') <- p2 s'
        return (f a, s'')

instance Monad Parser where
    return = pure
    (Parser p1) >>= f = Parser $ \s -> do
        (a, s') <- p1 s
        runParser (f a) s'

instance Alternative (Either ParseError) where
    empty = Left ParseError
    Left _ <|> e2 = e2
    Right a <|> _ = Right a

instance Alternative Parser where
    empty = Parser $ const empty
    (Parser p1) <|> (Parser p2) = Parser $ \s -> p1 s <|> p2 s

matchingCharP :: (Char -> Bool) -> Parser Char
matchingCharP f = Parser $ \s -> case s of
    (x:xs) | f x -> Right (x, xs)
    _ -> Left ParseError

peekCharP :: Char -> Parser Char
peekCharP c = Parser $ \s -> case s of
    (x:_) | x == c -> Right (x, s)
    _ -> Left ParseError

charP :: Char -> Parser Char
charP c = matchingCharP (== c)

isFilenameChar :: Char -> Bool
isFilenameChar c = isAlphaNum c || c `elem` "-_."

stringP :: String -> Parser String
stringP = mapM charP

filenameP :: Parser String
filenameP = some $ matchingCharP isFilenameChar

wsP :: Parser String
wsP = some $ matchingCharP isSpace

cwdP :: Parser Direction
cwdP = Here <$ charP '.'

parentP :: Parser Direction
parentP = Up <$ stringP ".."

rootP :: Parser Direction
rootP = Root <$ peekCharP '/'

childP :: Parser Direction
childP = Down <$> filenameP

pathTailElemP :: Parser Direction
pathTailElemP = parentP <|> cwdP <|> childP

pathHeadP :: Parser Direction
pathHeadP = rootP <|> pathTailElemP

pathTailP :: Parser [Direction]
pathTailP = many $ charP '/' *> pathTailElemP

pathP :: Parser [Direction]
pathP = (:) <$> pathHeadP <*> pathTailP

cdP :: Parser Command
cdP = Cd <$> (stringP "cd" *> wsP *> pathP)

lsP :: Parser Command
lsP = Ls <$ stringP "ls"

touchP :: Parser Command
touchP = Touch <$> (stringP "touch" *> wsP *> pathP)

mkdirP :: Parser Command
mkdirP = Mkdir <$> (stringP "mkdir" *> wsP *> pathP)

rmP :: Parser Command
rmP = Rm <$> (stringP "rm" *> wsP *> pathP)

commandP :: Parser Command
commandP = cdP <|> lsP <|> touchP <|> mkdirP <|> rmP