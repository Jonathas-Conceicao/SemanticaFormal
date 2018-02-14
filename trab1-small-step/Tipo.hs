module Tipo where

import Data.List as List

data Exp
  = Num Int
  | Var String
  | Som Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp

  | TRUE
  | FALSE
  | Not Exp
  | And Exp Exp
  | Or  Exp Exp
  | Ig  Exp Exp
  | Leq Exp Exp

  | Skip
  | Throw
  | Seq Exp Exp
  | Atrib Exp Exp
  | Catch Exp Exp
  | While Exp Exp
  | If Exp Exp Exp
  deriving (Eq, Show)

getArgs :: Exp -> [Exp]
getArgs (Not e) = [e]
getArgs (Som e1 e2) = [e1, e2]
getArgs (Sub e1 e2) = [e1, e2]
getArgs (Mul e1 e2) = [e1, e2]
getArgs (And e1 e2) = [e1, e2]
getArgs (Or  e1 e2) = [e1, e2]
getArgs (Ig  e1 e2) = [e1, e2]
getArgs (Leq e1 e2) = [e1, e2]
getArgs (Seq e1 e2) = [e1, e2]
getArgs (Atrib e1 e2) = [e1, e2]
getArgs (Catch e1 e2) = [e1, e2]
getArgs (While e1 e2) = [e1, e2]
getArgs (If e1 e2 e3) = [e1, e2, e3]
getArgs a = []

data Tipo
  = VOID
  | BOOL
  | INT
  deriving (Eq, Show)

data Tipo' = S Tipo | E String
  deriving (Eq)

instance Show Tipo' where
  show (S t) = show t
  show (E s) = show s

getTipo :: Tipo' -> Tipo
getTipo (S t) = t
getTipo (E s) = errorWithoutStackTrace s

isError :: Tipo' -> Bool
isError (E _) = True
isError (S _) = False

getError :: Tipo' -> String
getError (E s) = s

isFinal :: Exp -> Bool
isFinal (Num a) = True
isFinal TRUE  = True
isFinal FALSE = True
isFinal Throw = True
isFinal Skip  = True
isFinal exp = False

erroDeTipo :: Exp -> Tipo' -> Tipo' -> Exp -> String
erroDeTipo ex te tr outter = "Panico! Erro de tipo. \n" ++
  (show ex) ++ " tem tipo " ++
  (show tr) ++ ", mas era esperado tipo " ++
  (show te) ++ "\nNa expressão " ++
  (show outter) ++ "\n"

naExpressao :: String -> Exp -> String
naExpressao s e = (s ++) $ "Na expressão " ++ (show e) ++ "\n"

iTipo :: Exp -> Tipo
iTipo e = getTipo $ iTipo' e

iTipo' :: Exp -> Tipo'
iTipo' (Num x) = S INT
iTipo' (Var x) = S INT
iTipo' TRUE    = S BOOL
iTipo' FALSE   = S BOOL
iTipo' Skip    = S VOID
iTipo' Throw   = S VOID
iTipo' exp@(Som _ _) = testarTipos exp (getArgs exp) [S INT, S INT, S INT]
iTipo' exp@(Sub _ _) = testarTipos exp (getArgs exp) [S INT, S INT, S INT]
iTipo' exp@(Mul _ _) = testarTipos exp (getArgs exp) [S INT, S INT, S INT]
iTipo' exp@(Not  _ ) = testarTipos exp (getArgs exp) [S BOOL, S BOOL]
iTipo' exp@(And _ _) = testarTipos exp (getArgs exp) [S BOOL, S BOOL, S BOOL]
iTipo' exp@(Or  _ _) = testarTipos exp (getArgs exp) [S BOOL, S BOOL, S BOOL]
iTipo' exp@(Ig  _ _) = testarTipos exp (getArgs exp) [S INT, S INT, S BOOL]
iTipo' exp@(Leq _ _) = testarTipos exp (getArgs exp) [S INT, S INT, S BOOL]
iTipo' exp@(Seq _ _) = testarTipos exp (getArgs exp) [S VOID, S VOID, S VOID]
iTipo' exp@(Atrib _ _) = testarTipos exp (getArgs exp) [S INT, S INT, S VOID]
iTipo' exp@(Catch _ _) = testarTipos exp (getArgs exp) [S VOID, S VOID, S VOID]
iTipo' exp@(While _ _) = testarTipos exp (getArgs exp) [S BOOL, S VOID, S VOID]
iTipo' exp@(If  _ _ _) = testarTipos exp (getArgs exp) [S BOOL, S VOID, S VOID, S VOID]

testarTipos :: Exp -> [Exp] -> [Tipo'] -> Tipo'
testarTipos exp [] (t:ts) = t
testarTipos exp (e:es) (t:ts)
  | te == t      = testarTipos exp es ts
  | isError te   = E $ naExpressao (getError te) exp
  | otherwise    = E $ erroDeTipo e t te exp
  where
    te = iTipo' e

