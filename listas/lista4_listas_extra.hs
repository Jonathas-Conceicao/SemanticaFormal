membro :: (Eq a) => [a] -> a -> Bool
membro [] _ = False
membro (x:xs) a
  | x == a    = True
  | otherwise = membro xs a
