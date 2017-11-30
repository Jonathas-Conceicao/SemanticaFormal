module Estado where

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

adiciona :: Memoria a -> String -> Int -> Memoria ()
adiciona m s x = do
  vs <- getVar
  putVar ((s,x):vs)

-- findVar :: Variaveis -> String ->

encontraVar :: Variaveis -> String -> Int
encontraVar [] s = error ("Variavel " ++ s ++ " nao definida no estado")
encontraVar ((s,i):xs) v
  | s == v     = i
  | otherwise  = encontraVar xs v

procuraVar :: String -> Memoria Int
procuraVar s = do
  var <- getVar
  let x = encontraVar var s
  return x

alteraVar :: String -> Int -> Memoria ()
alteraVar s i = do
  var <- getVar
  let newM = mudaVar var s i
  putVar newM
  return ()

mudaVar :: Variaveis -> String -> Int -> Variaveis
mudaVar [] v n = error ("Variavel " ++ v ++ " nao definida no estado")
mudaVar ((s,i):xs) v n
  | s == v     = ((s,n):xs)
  | otherwise  = (s,i): mudaVar xs v n
