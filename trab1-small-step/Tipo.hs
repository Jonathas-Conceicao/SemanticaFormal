module Tipo where

import qualified Data.List as List

data AExp
  = Num Int
  | Var String
  | Som AExp AExp
  | Sub AExp AExp
  | Mul AExp AExp
  deriving (Eq, Show)

data BExp
  = TRUE
  | FALSE
  | Not BExp
  | And BExp BExp
  | Or  BExp BExp
  | Ig  AExp AExp
  | Leq AExp AExp
  deriving (Eq, Show)

data CExp
  = While BExp CExp
  | If BExp CExp CExp
  | Seq CExp CExp
  | Atrib AExp AExp
  | Catch CExp CExp
  | Throw
  | Skip
  deriving (Eq, Show)

data Tipo
  = VOID
  | BOOL
  | INT
  deriving (Eq, Show)

getTipoVar :: [(AExp, Tipo)] -> AExp -> Tipo
getTipoVar list exp = case List.lookup exp list of
  Just x -> x
  Nothing -> error $ "Expression " ++ (show exp) ++ " not found!"


listaTipos :: [(AExp, Tipo)]
listaTipos = [(Var "x", INT), (Var "y", INT), (Var "b", BOOL)]
