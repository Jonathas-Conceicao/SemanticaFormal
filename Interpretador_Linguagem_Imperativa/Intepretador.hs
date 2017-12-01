module Interpretador where

import Memoria

data AExp = Num  Int
          | Var  String
          | Som  AExp AExp
          | Sub  AExp AExp
          | Mult AExp AExp
          deriving (Show, Eq)

data BExp = TRUE
          | FALSE
          | Not BExp
          | And BExp BExp
          | Leq BExp BExp
          deriving (Show, Eq)

data CExp = Skip
          | Seq   CExp CExp
          | Atrib AExp AExp
          | If    BExp CExp CExp
          deriving (Show, Eq)

aBigStep :: AExp -> Memoria (Int)
aBigStep (Num x) = return x
aBigStep (Var s) = procuraVar s
aBigStep (Som e1 e2) = do
  x <- aBigStep e1
  y <- aBigStep e2
  return (x+y)
