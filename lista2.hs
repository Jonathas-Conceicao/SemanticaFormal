import Prelude hiding (max)

type Semana = Int

venda :: Semana -> Int
venda n = n

max :: Ord a => a -> a -> a
max a b
  | a >= b    = a
  | otherwise = b

maiorVenda :: Semana -> Int
maiorVenda 0 = venda 0
maiorVenda n = max (venda n) (maiorVenda (n-1))

maxVenda :: Semana -> Int
maxVenda = error "To Do"

zeroVendas :: Semana -> Int
zeroVendas = nVendas 0

nVendas :: Int -> Semana -> Int
nVendas _ (-1) = -1
nVendas s n = if venda n == s
  then n
  else nVendas (n-1) s

fat :: Integer -> Integer
fat 1 = 1
fat n = n * fat (n-1)

produtao :: Integer -> Integer -> Integer
produtao m n = product [m .. n]

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
