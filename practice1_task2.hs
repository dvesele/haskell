isPrime :: Int -> Bool
isPrime n
   | n <= 1 = False
   | otherwise = not $ any (\x -> n `mod` x == 0) [2..intSqrt n]
   where
      intSqrt = floor . sqrt . fromIntegral