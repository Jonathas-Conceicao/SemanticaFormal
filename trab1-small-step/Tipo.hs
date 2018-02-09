module Tipo where

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
naExpressao s e = (s ++) $ "Na expressãoD " ++ (show e) ++ "\n"

iTipo :: Exp -> Tipo
iTipo e = getTipo $ iTipo' e

iTipo' :: Exp -> Tipo'
iTipo' (Num x) = S INT
iTipo' (Var x) = S INT
iTipo' TRUE    = S BOOL
iTipo' FALSE   = S BOOL
iTipo' Skip    = S VOID
iTipo' Throw   = S VOID
iTipo' arg@(Som e1 e2) = case e1t of
  S INT -> case e2t of
    S INT -> S INT
    E s -> E $ naExpressao s arg
    otherwise -> E $ erroDeTipo e2 (S INT) e2t arg
  E s -> E $ naExpressao s arg
  otherwise -> E $ erroDeTipo e1 (S INT) e1t arg
  where
    e1t = iTipo' e1
    e2t = iTipo' e2
iTipo' arg@(Sub e1 e2) = case e1t of
  S INT -> case e2t of
    S INT -> S INT
    E s -> E $ naExpressao s arg
    otherwise -> E $ erroDeTipo e2 (S INT) e2t arg
  E s -> E $ naExpressao s arg
  otherwise -> E $ erroDeTipo e1 (S INT) e1t arg
  where
    e1t = iTipo' e1
    e2t = iTipo' e2
iTipo' arg@(Mul e1 e2) = case e1t of
  S INT -> case e2t of
    S INT -> S INT
    E s -> E $ naExpressao s arg
    otherwise -> E $ erroDeTipo e2 (S INT) e2t arg
  E s -> E $ naExpressao s arg
  otherwise -> E $ erroDeTipo e1 (S INT) e1t arg
  where
    e1t = iTipo' e1
    e2t = iTipo' e2
iTipo' arg@(Not e1) = case e1t of
  S BOOL -> S BOOL
  E s -> E $ naExpressao s arg
  otherwise -> E $ erroDeTipo e1 (S BOOL) e1t arg
  where
    e1t = iTipo' e1
iTipo' arg@(And e1 e2) = case e1t of
  S BOOL -> case e2t of
    S BOOL -> S BOOL
    E s -> E $ naExpressao s arg
    otherwise -> E $ erroDeTipo e2 (S BOOL) e2t arg
  E s -> E $ naExpressao s arg
  otherwise -> E $ erroDeTipo e1 (S BOOL) e1t arg
  where
    e1t = iTipo' e1
    e2t = iTipo' e2
iTipo' arg@(Or e1 e2) = case e1t of
  S BOOL -> case e2t of
    S BOOL -> S BOOL
    E s -> E $ naExpressao s arg
    otherwise -> E $ erroDeTipo e2 (S BOOL) e2t arg
  E s -> E $ naExpressao s arg
  otherwise -> E $ erroDeTipo e1 (S BOOL) e1t arg
  where
    e1t = iTipo' e1
    e2t = iTipo' e2
iTipo' arg@(Ig e1 e2) = case e1t of
  S INT -> case e2t of
    S INT -> S INT
    E s -> E $ naExpressao s arg
    otherwise -> E $ erroDeTipo e2 (S INT) e2t arg
  E s -> E $ naExpressao s arg
  otherwise -> E $ erroDeTipo e1 (S INT) e1t arg
  where
    e1t = iTipo' e1
    e2t = iTipo' e2
iTipo' arg@(Leq e1 e2) = case e1t of
  S INT -> case e2t of
    S INT -> S INT
    E s -> E $ naExpressao s arg
    otherwise -> E $ erroDeTipo e2 (S INT) e2t arg
  E s -> E $ naExpressao s arg
  otherwise -> E $ erroDeTipo e1 (S INT) e1t arg
  where
    e1t = iTipo' e1
    e2t = iTipo' e2
iTipo' arg@(Seq e1 e2) = case e1t of
  S VOID -> case e2t of
    S VOID -> S VOID
    E s -> E $ naExpressao s arg
    otherwise -> E $ erroDeTipo e2 (S VOID) e2t arg
  E s -> E $ naExpressao s arg
  otherwise -> E $ erroDeTipo e1 (S VOID) e1t arg
  where
    e1t = iTipo' e1
    e2t = iTipo' e2
iTipo' arg@(Atrib e1 e2) = case e1t of
  S INT -> case e2t of
    S VOID -> S VOID
    E s -> E $ naExpressao s arg
    otherwise -> E $ erroDeTipo e2 (S VOID) e2t arg
  E s -> E $ naExpressao s arg
  otherwise -> E $ erroDeTipo e1 (S INT) e1t arg
  where
    e1t = iTipo' e1
    e2t = iTipo' e2
iTipo' arg@(While e1 e2) = case e1t of
  S BOOL -> case e2t of
    S VOID -> S VOID
    E s -> E $ naExpressao s arg
    otherwise -> E $ erroDeTipo e2 (S VOID) e2t arg
  E s -> E $ naExpressao s arg
  otherwise -> E $ erroDeTipo e1 (S BOOL) e1t arg
  where
    e1t = iTipo' e1
    e2t = iTipo' e2
iTipo' arg@(If e1 e2 e3) = case e1t of
  S BOOL -> case e2t of
    S VOID -> case e3t of
      S VOID -> S VOID
      E s -> E $ naExpressao s arg
      otherwise -> E $ erroDeTipo e2 (S VOID) e3t arg 
    E s -> E $ naExpressao s arg
    otherwise -> E $ erroDeTipo e2 (S VOID) e2t arg
  E s -> E $ naExpressao s arg
  otherwise -> E $ erroDeTipo e1 (S BOOL) e1t arg
  where
    e1t = iTipo' e1
    e2t = iTipo' e2
    e3t = iTipo' e3
