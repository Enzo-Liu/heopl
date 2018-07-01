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

tokenEnd :: Parser Char
tokenEnd = noneOf " )"

aint :: Parser Atom
aint = AInt <$> int <* notFollowedBy tokenEnd

abool :: Parser Atom
abool = ABool <$> bool <* notFollowedBy tokenEnd

astring :: Parser Atom
astring = AString <$> many1 alphaNum

atom :: Parser Atom
atom = choice [try aint, try abool, astring]

satom :: Parser SExpr
satom = SAtom <$> atom

spaces1 :: Parser Char
spaces1 = space <* spaces

slist :: Parser SExpr
slist = SList <$> between (char '(') (char ')')
        (sepBy1 (satom <|> slist) spaces1)

sexp :: Parser SExpr
sexp = slist <|> satom
