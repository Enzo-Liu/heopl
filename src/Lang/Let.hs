module Lang.Let where

import           Data.Default
import           Data.Map.Strict        (Map, insert, (!))
import           Text.Parsec
import           Text.Parsec.ByteString
import           Text.Parsec.Number

newtype Identity = Identity String deriving (Show, Eq, Ord)
data Expr = ENum Int
          | EDiff Expr Expr
          | EAdd Expr Expr
          | EMul Expr Expr
          | EQuot Expr Expr
          | EMinus Expr
          | EEmptyList
          | ECons Expr Expr
          | EList [Expr]
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
ediff = op "-" EDiff

eadd :: Parser Expr
eadd = op "+" EAdd

emul :: Parser Expr
emul = op "*" EMul

equot :: Parser Expr
equot = op "/" EQuot

op :: String -> (Expr -> Expr -> Expr) -> Parser Expr
op s operator = string s >> spaces >> brackets
  (operator <$> expr <* spaces <* char ',' <* spaces <*> expr)

eif :: Parser Expr
eif = EIf <$> keyExpr "if" <*> keyExpr "then" <*> keyExpr "else"

keyParser :: String -> Parser a -> Parser a
keyParser key a = string key >> spaces *> a <* spaces

keyExpr :: String -> Parser Expr
keyExpr key = keyParser key expr

eminus :: Parser Expr
eminus = string "minus" >> spaces >> brackets (EMinus <$> expr)

ezero :: Parser Expr
ezero = string "zero?" >> spaces >> brackets (EZero <$> expr)

evar :: Parser Expr
evar = EVar <$> (Identity <$> many1 letter)

elet :: Parser Expr
elet = ELet <$> keyParser "let" (Identity <$> many1 letter) <*>
       keyExpr "=" <*> keyExpr "in"

eempty :: Parser Expr
eempty = string "emptylist" >> return EEmptyList

econs :: Parser Expr
econs = op "cons" ECons

elist :: Parser Expr
elist = EList <$> (string "list" >> spaces >> brackets (sepBy expr (char ',')))

-- parseTest expr "-(55, -(x,11))"
-- parseTest expr "let z = 5 in let x = 3 in let y = -(x,1) in let x = 4 in -(z, -(x,y))"
expr :: Parser Expr
expr = choice [ try enum
              , try ediff
              , try eadd
              , try emul
              , try equot
              , try ezero
              , try eif
              , try elet
              , try eminus
              , try eempty
              , try econs
              , try elist
              , evar
              ]


newtype Env = Env (Map Identity Val) deriving (Show, Eq)
data VList = VEmpty | VCon Val VList deriving (Show, Eq)
data Val = ValI Int | ValB Bool | ValL VList deriving (Show, Eq)

instance Default Env where
  def = Env def

-- eval <$> runParser expr () "123" "let z = 5 in let x = 3 in let y = -(x,1) in let x = 4 in -(z, -(x,y))"
-- eval <$> runParser expr () "123" "let x = 4 in list(x,-(x,1),-(x,3))"
eval :: Expr -> Val
eval = evalEnv def

type OP = Int -> Int -> Int
apply :: Env -> OP -> Expr -> Expr -> Val
apply env operator e1 e2 =
  let (ValI i1) = evalEnv env e1
      (ValI i2) = evalEnv env e2
  in ValI (operator i1 i2)

evalEnv :: Env -> Expr -> Val
evalEnv _ (ENum i) = ValI i
evalEnv env (EDiff e1 e2) = apply env (-) e1 e2

evalEnv env (EAdd e1 e2) = apply env (+) e1 e2

evalEnv env (EMul e1 e2) = apply env (*) e1 e2

evalEnv env (EQuot e1 e2) = apply env quot e1 e2
evalEnv env (EMinus e) = let (ValI i) = evalEnv env e in ValI (-i)
evalEnv env (EZero e) =
  if ValI 0 == evalEnv env e then ValB True else ValB False

evalEnv env (EIf e1 e2 e3) =
  let (ValB pred') = evalEnv env e1
  in if pred' then evalEnv env e2 else evalEnv env e3

evalEnv (Env curEnv) (EVar ident) = curEnv ! ident
evalEnv env (ECons e1 e2) =
  let v1 = evalEnv env e1
      (ValL v2) = evalEnv env e2
  in ValL (VCon v1 v2)

evalEnv _ EEmptyList = ValL VEmpty
evalEnv env (EList exprlist) =
  let vallist = map (evalEnv env) exprlist
  in ValL $ foldr VCon VEmpty vallist

evalEnv env@(Env curEnv) (ELet ident e1 e2) =
  let nextEnv = insert ident (evalEnv env e1) curEnv
  in evalEnv (Env nextEnv) e2
