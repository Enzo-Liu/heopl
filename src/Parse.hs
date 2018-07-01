{-# LANGUAGE OverloadedStrings #-}
module Parse where

import Text.Parsec
import Text.Parsec.ByteString
import Text.Parsec.Number

data Atom = AInt Int | ABool Bool | AString String deriving (Show, Eq)
data SExpr = SAtom Atom | SList [SExpr] deriving (Show, Eq)

true :: Parser Bool
true = string "true" *> return True

false :: Parser Bool
false = string "false" *> return False

bool :: Parser Bool
bool = true <|> false

aint :: Parser Atom
aint = AInt <$> int

abool :: Parser Atom
abool = ABool <$> bool

astring :: Parser Atom
astring = AString <$> many1 alphaNum

atom :: Parser Atom
atom = aint <|> abool <|> astring

satom :: Parser SExpr
satom = SAtom <$> atom

slist :: Parser SExpr
slist = SList <$> between (char '(') (char ')')
        (many $ (satom <|> slist) <* spaces)

-- TODO: fix 3s true1 ..
-- parseTest sexp "(1 2 sdf1 3s (1 true1))"
sexp :: Parser SExpr
sexp = slist <|> satom
