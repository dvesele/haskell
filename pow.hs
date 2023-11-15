import Data.Bits

powOf2 :: Integer -> Int
powOf2 n
   | n <= 0 = -1 -- Если число не положительное, возвращаем -1
   | n == 1 = 0 -- 1 является 2^0
   | otherwise = if isPowerOf2 n then findExponent n 0 else -1

-- Проверка, является ли число степенью двойки
isPowerOf2 :: Integer -> Bool
isPowerOf2 n = n /= 0 && (n .&. (n - 1)) == 0

-- Поиск степени числа двойки
findExponent :: Integer -> Int -> Int
findExponent 1 exponent = exponent
findExponent n exponent = findExponent (n `div` 2) (exponent + 1)