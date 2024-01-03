import RSA hiding (main)
import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)

-- Property: e * d ≡ 1 (mod φ(n)) for valid e, d, and φ(n)
prop_modularInverse :: Integer -> Integer -> Property
prop_modularInverse p q =
  (p > 1 && q > 1 && p /= q && prime p && prime q) ==>
    let phi = computeEulerFunction p q
        e = selectE phi
        d = computeD e phi
     in (e * d) `mod` phi == 1

-- Sample prime generator for testing
genPrime :: Gen Integer
genPrime = oneof $ map return [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]

-- Run QuickCheck test
main :: IO ()
main = quickCheck $ forAll genPrime $ \p -> forAll genPrime $ \q -> prop_modularInverse p q
