module Main where -- объявление модуля, который будет использовать для ввода данных в консоли

import System.Environment -- импорт System.Environment для использования функции getArgs (https://hackage.haskell.org/package/base-4.19.0.0/docs/System-Environment.html) - опять же для красивого ввода в консоль

root :: (Double -> Double) -> Double -> Double -> Double -> Double
root f a b eps
   | f a * f b > 0 = error "На заданном интервале функция не меняет знак" 
   | otherwise = bisect a b
   where
      bisect :: Double -> Double -> Double -- реализуем бисекцию
      bisect x y
         | y - x < eps = (x + y) / 2 --если интервал меньше eps, то среднее значение
         | f m * f x < 0 = bisect x m
         | otherwise = bisect m y
         where
            m = (x + y) / 2

main :: IO ()
main = do
   args <- getArgs -- получаем аргументы командной строки
   case args of
      [expression, a, b, eps] -> do
         let f x = read expression :: Double
            a' = read a :: Double
            b' = read b :: Double
            eps' = read eps :: Double
            result = root f a' b' eps'
         putStrLn $ "Найденный корень: " ++ show result
         -- вывод результата на экран
      _ -> putStrLn "Использование: root 'функция' 'левая граница' 'правая граница' 'точность'" -- *Main> root (\x -> x^3 - 9) 1.0 3.0 0.001 
