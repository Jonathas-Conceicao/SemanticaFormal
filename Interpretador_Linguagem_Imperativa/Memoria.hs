module Memoria
  ( Memoria
  , adicionaVar
  , procuraVar
  , alteraVar
  , evalProgram
  ) where

type Variaveis = [(String, Int)]

data Memoria a = Memoria (Variaveis -> (a, Variaveis))

instance Functor Memoria where
  fmap f m = do
    r <- m
    return (f r)

instance Applicative Memoria where
  pure = return
  fm <*> mx = do
    f <- fm
    x <- mx
    return (f x)

instance Monad Memoria where
  return x = Memoria $ \s -> (x,s)
  (Memoria h) >>= f = Memoria $ \s -> let (a, newS) = h s
                                          (Memoria g) = f a
                                      in g newS

getVar :: Memoria Variaveis
getVar = Memoria $ \s -> (s, s)

putVar :: Variaveis -> Memoria ()
putVar s = Memoria $ \ _ -> ((), s)

evalProgram :: Memoria a -> Variaveis -> a
evalProgram (Memoria p) mem = fst $ p mem

adicionaVar :: String -> Int -> Memoria ()
adicionaVar s x = do
  vs <- getVar
  putVar ((s,x):vs)

procuraVar :: String -> Memoria Int
procuraVar s = do
  vars <- getVar
  let x = procuraVarAux vars s
  return x

procuraVarAux :: Variaveis -> String -> Int
procuraVarAux [] s = error ("Variavel " ++ s ++ " nao definida no estado")
procuraVarAux ((s,i):xs) v
  | s == v     = i
  | otherwise  = procuraVarAux xs v

alteraVar :: String -> Int -> Memoria ()
alteraVar s i = do
  vars <- getVar
  let newM = alteraVarAux vars s i
  putVar newM
  return ()

alteraVarAux :: Variaveis -> String -> Int -> Variaveis
alteraVarAux [] v n = error ("Variavel " ++ v ++ " nao definida no estado")
alteraVarAux ((s,i):xs) v n
  | s == v     = ((s,n):xs)
  | otherwise  = (s,i): alteraVarAux xs v n
