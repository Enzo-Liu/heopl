module Lang.Let where

import Text.Parsec
import Text.Parsec.ByteString
import Text.Parsec.Number

newtype Identity = Identity String deriving (Show, Eq)
data Expr = ENum Int
          | EDiff Expr Expr
          | EZero Expr
          | EIf Expr Expr Expr
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

eif :: Parser Expr
eif = EIf <$> keyExpr "if" <*> keyExpr "then" <*> keyExpr "else"

keyParser :: String -> Parser a -> Parser a
keyParser key a = string key >> spaces *> a <* spaces

keyExpr :: String -> Parser Expr
keyExpr key = keyParser key expr

ezero :: Parser Expr
ezero = string "zero?" >> spaces >> brackets (EZero <$> expr)

evar :: Parser Expr
evar = EVar <$> (Identity <$> many1 letter)

elet :: Parser Expr
elet = ELet <$> keyParser "let" (Identity <$> many1 letter) <*>
       keyExpr "=" <*> keyExpr "in"

-- parseTest expr "-(55, -(x,11))"
-- parseTest expr "let z = 5 in let x = 3 in let y = -(x,1) in let x = 4 in -(z, -(x,y))"
expr :: Parser Expr
expr = choice [try enum, try ediff, try ezero, try eif, try elet, evar]
