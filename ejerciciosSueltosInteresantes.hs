
-- Pattern matching --
xor :: Bool -> Bool -> Bool
xor True True = False
xor bool1 bool2 = bool1 || bool2   -- Se comporta como un or logico sacando el primer caso


-- Recursividad --
fibonacci :: Int -> Int
fibonacci 0 = 0   -- Siempre para recursividad se necesita un caso base
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)  -- secuencia fibonacci: F(n) = F(n-1) + F(n-2)


factorial :: Int -> Int
factorial 0 = 1
factorial n
        | n > 0 = n * factorial (n-1)


-- Tipos --
diferenciaMayorA :: (Num a, Ord a) => a -> a -> Bool
diferenciaMayorA x y = x - y > 100