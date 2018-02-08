module Tipo where

import qualified Data.List as List

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
  | While Exp Exp
  | If Exp Exp Exp
  | Seq Exp Exp
  | Atrib Exp Exp
  | Catch Exp Exp
  | Throw
  | Skip
  deriving (Eq, Show)

data Tipo
  = VOID
  | BOOL
  | INT
  deriving (Eq, Show)

