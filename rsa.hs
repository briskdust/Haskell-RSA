prime :: Integer -> Bool
prime n = n > 1 && all (\x -> rem n x /= 0) xs
  where
    xs = takeWhile (\y -> y ^ 2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3 ..]

computeEulerFunction :: Integer -> Integer -> Integer
computeEulerFunction p q = (p - 1) * (q - 1)

selectE :: Integer -> Integer
selectE phi = head [e | e <- [2 ..], gcd e phi == 1]

computeD :: Integer -> Integer -> Integer
computeD = modInverse

extendedEuclid :: Integer -> Integer -> (Integer, Integer)
extendedEuclid a b
  | b == 0 = (1, 0)
  | otherwise = (t, s - q * t)
  where
    (q, r) = quotRem a b
    (s, t) = extendedEuclid b r

modInverse :: Integer -> Integer -> Integer
modInverse a m = let (x, _) = extendedEuclid a m in (x `mod` m)

encapsulatePub :: Integer -> Integer -> (Integer, Integer)
encapsulatePub n e = (n, e)

encapsulatePriv :: Integer -> Integer -> (Integer, Integer)
encapsulatePriv n d = (n, d)

main :: IO ()
main = print $ computeD 17 3120