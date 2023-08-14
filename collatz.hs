collatz :: Integer -> Maybe Integer
collatz n
    | n<=0 = Nothing
    | otherwise = Just (toInteger (ma_collatz n []))


ma_collatz :: Integer -> [Integer] -> Int
ma_collatz n lst 
   | n == 1 = length lst
   | (testcond == 0) = ma_collatz (caseDivideby2) (caseDivideby2:lst)
   | (testcond /= 0) = ma_collatz (otherCase) (otherCase:lst)
   where caseDivideby2= div n 2
         otherCase = n*3+1
	 testcond = n `mod` 2


-- Solução da comunidade, BEEM melhor e BEEM mais elegante
collatz2 :: Integer -> Maybe Integer
collatz2 n | n <= 0 = Nothing
          | n == 1 = Just 0
          | even n     = succ <$> collatz (n `div` 2)
          | otherwise  = succ <$> collatz (3 * n + 1)
