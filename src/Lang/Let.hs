module Lang.Let where

import           Data.Default
import           Data.Map.Strict        (Map, insert, (!))
import           Text.Parsec
import           Text.Parsec.ByteString
import           Text.Parsec.Number

newtype Identity = Identity String deriving (Show, Eq, Ord)
data Expr = ENum Int
          | EDiff Expr Expr
          | EMinus Int
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

eminus :: Parser Expr
eminus = string "minus" >> spaces >> brackets (EMinus <$> int)

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
expr = choice [ try enum
              , try ediff
              , try ezero
              , try eif
              , try elet
              , try eminus
              , evar
              ]


newtype Env = Env (Map Identity Val) deriving (Show, Eq)
data Val = ValI Int | ValB Bool deriving (Show, Eq)

instance Default Env where
  def = Env def

-- eval <$> runParser expr () "123" "let z = 5 in let x = 3 in let y = -(x,1) in let x = 4 in -(z, -(x,y))"
eval :: Expr -> Val
eval = evalEnv def

evalEnv :: Env -> Expr -> Val
evalEnv _ (ENum i) = ValI i
evalEnv env (EDiff e1 e2) =
  let (ValI i1) = evalEnv env e1
      (ValI i2) = evalEnv env e2
  in ValI (i1 - i2)
evalEnv _ (EMinus i) = ValI (-i)
evalEnv env (EZero e) =
  if ValI 0 == evalEnv env e then ValB True else ValB False

evalEnv env (EIf e1 e2 e3) =
  let (ValB pred') = evalEnv env e1
  in if pred' then evalEnv env e2 else evalEnv env e3

evalEnv (Env curEnv) (EVar ident) = curEnv ! ident
evalEnv env@(Env curEnv) (ELet ident e1 e2) =
  let nextEnv = insert ident (evalEnv env e1) curEnv
  in evalEnv (Env nextEnv) e2
