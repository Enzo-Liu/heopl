module Lang.Let where

import Text.Parsec
import Text.Parsec.ByteString
import Text.Parsec.Number

newtype Identity = Identity String deriving (Show, Eq)
data Expr = ENum Int
          | EDiff Expr Expr
          | EZero Expr
          | EIF Expr Expr Expr
          | EVar Identity
          | ELet Identity Expr Expr
  deriving (Show, Eq)

enum :: Parser Expr
enum = ENum <$> int

etuple :: Parser (Expr, Expr)
etuple = (,) <$> expr <* spaces <* char ',' <* spaces <*> expr

brackets :: Parser a -> Parser a
brackets = between (char '(') (char ')')

ediff :: Parser Expr
ediff = char '-' >> spaces >> brackets
  (EDiff <$> expr <* spaces <* char ',' <* spaces <*> expr)

ezero :: Parser Expr
ezero = string "zero?" >> spaces >> brackets (EZero <$> expr)

evar :: Parser Expr
evar = EVar <$> (Identity <$> many1 letter)

-- parseTest expr "-(55, -(x,11))"
expr :: Parser Expr
expr = choice [try enum, try ediff, try ezero, evar]
