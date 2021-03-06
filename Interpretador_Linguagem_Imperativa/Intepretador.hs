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
          | Or  BExp BExp
          | Leq AExp AExp
          | Eq  AExp AExp
          deriving (Show, Eq)

data CExp = Skip
          | Seq   CExp CExp
          | Atrib AExp AExp
          | If    BExp CExp CExp
          | While BExp CExp
          deriving (Show, Eq)

aBigStep :: AExp -> Memoria (Int)
aBigStep (Num x) = return x
aBigStep (Var s) = procuraVar s
aBigStep (Som e1 e2) = do
  x <- aBigStep e1
  y <- aBigStep e2
  return (x + y)
aBigStep (Sub e1 e2) = do
  x <- aBigStep e1
  y <- aBigStep e2
  return (x - y)
aBigStep (Mult e1 e2) = do
  x <- aBigStep e1
  y <- aBigStep e2
  return (x * y)

bBigStep :: BExp -> Memoria (Bool)
bBigStep TRUE  = return (True)
bBigStep FALSE = return (False)
bBigStep (Not b)     = do
  x <- bBigStep b
  return (not x)
bBigStep (And b1 b2) = do
  x <- bBigStep b1
  y <- bBigStep b2
  return (x && y)
bBigStep (Or  b1 b2) = do
  x <- bBigStep b1
  y <- bBigStep b2
  return (x || y)
bBigStep (Leq a1 a2) = do
  x <- aBigStep a1
  y <- aBigStep a2
  return (x <= y)
bBigStep (Eq  a1 a2) = do
  x <- aBigStep a1
  y <- aBigStep a2
  return (x == y)

cBigStep :: CExp -> Memoria (CExp)
cBigStep Skip              = return Skip
cBigStep (Atrib (Var s) a) = do
  x <- aBigStep a
  alteraVar s x
  return Skip
cBigStep (Seq c1 c2) = do
  cBigStep c1
  cBigStep c2
  return Skip
cBigStep (If b c1 c2) = do
  x <- bBigStep b
  case x of
    True  -> do
      cBigStep c1
      return Skip
    False -> do
      cBigStep c2
      return Skip
cBigStep (While b c) = do
  x <- bBigStep b
  case x of
    True  -> do
      cBigStep c
      cBigStep (While b c)
    False -> do
      return Skip

prog1 :: CExp
prog1 = Seq (Atrib (Var "x") (Num 3))
            (Seq (Atrib (Var "y") (Num 2))
                 (Atrib (Var "z") (Som (Var "x") (Var "y")) ))
prog2 :: CExp
prog2 = Seq (Atrib (Var "i") (Num 5))
            (While (Not (Eq (Var "i") (Num 0)))
                   (Atrib (Var "i") (Sub (Var "i") (Num 1))))

prog3 :: CExp
prog3 = Seq (Atrib (Var "i") (Num 0))
            (Atrib (Var "i") (Som (Var "i") (Num 1)))
