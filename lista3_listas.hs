dobraLista :: (Num a) => [a] -> [a]
dobraLista = map (*2)

tamanho :: [a] -> Int
tamanho = foldl (\x _ -> (x + 1)) 0

produtoLista :: (Num a) => [a] -> a
produtoLista (x:xs) = foldl (*) x xs

andLista :: [Bool] -> Bool
andLista (x:xs) = foldl (&&) x xs

concatLista :: [[a]] -> [a]
concatLista (x:xs) = foldl (++) x xs

inverteLista :: [a] -> [a]
inverteLista l = inv l []
  where
    inv [] r = r
    inv (x:xs) r = inv xs (x:r)
