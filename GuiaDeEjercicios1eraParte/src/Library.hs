module Library where
import PdePreludat

-- Devolver el signo de un numero --
signo :: Number -> Char
signo 0 = '0'
signo numero
 | numero > 0 = '+'
 | otherwise = '-'

-- Recibir cadena de clima y devolver probabilidad --
probabilidadDeLluvia :: String -> Number
probabilidadDeLluvia "Despejado" = 0
probabilidadDeLluvia "Nublado" = 25
probabilidadDeLluvia "Lluvioso" = 100
probabilidadDeLluvia _ = 50


-- Dados 3 elementos, devolver el mayor de los 3 --
mayorDeTres :: Ord a => a -> a -> a -> a       
-- indico que el tipo a pertenece a la clase Ord, lo que me habilita a usar operadores de comparacion como >=, <=, > y <
mayorDeTres elem1 elem2 elem3 = max elem1 (max elem2 elem3)

-- Que año es bisiesto --
bisiesto :: Number -> Bool
bisiesto anio = (esMultiploDe 4 anio && esMultiploDe 100 anio) || esMultiploDe 400 anio

-- Saber si un numero es multiplo de 3 --
esMultiploDeTres :: Number -> Bool
esMultiploDeTres numero = numero `mod` 3 == 0

-- Saber si un numero es multiplo de otro --
esMultiploDe :: Number -> Number -> Bool
esMultiploDe dividendo divisor = dividendo `mod` divisor == 0

-- Calcular Area de un rectangulo --
areaRectangulo :: Number -> Number -> Number
areaRectangulo base altura = base * altura

-- Volumen de un cubo --
volumenCubo :: Number -> Number
volumenCubo base = base * base * base


-- Calcular las superficies de cada color (Guia) --
-- Cuadrado, circulo, cuadrado menor --
areaCuadrado :: Number -> Number
areaCuadrado x = x * x
-- areaCuadrado x = areaRectangulo x x

areaCirculo :: Number -> Number
areaCirculo x = pi * (x ^ 2)

areaCuadradoMenor :: Number -> Number
areaCuadradoMenor x = areaCuadrado (sqrt ((x/2)^2 + (x/2)^2))  -- Pitagoras


-- Conversion de temperaturas --
celsiusToFahr :: Number -> Number
celsiusToFahr celsius = (9/5) * celsius + 32

fahrToCelsius :: Number -> Number
fahrToCelsius fahr = (fahr - 32) * (5/9)

haceFrioF :: Number -> Bool
haceFrioF fahr = fahrToCelsius fahr < 8


-- Un número entero es cuadrado perfecto, si es el cuadrado de algún número entero. --
esCuadradoPerfecto :: Number -> Bool
esCuadradoPerfecto numero = (floor (sqrt  numero) ^ 2) == numero

cuadradoPerfectoMayorQue :: Number -> Number
cuadradoPerfectoMayorQue numero
  | esCuadradoPerfecto (numero+1) = numero + 1
  | otherwise = cuadradoPerfectoMayorQue (numero+1)   -- Recursividad

cantidadCuadradosPerfectos :: Number -> Number -> Number
cantidadCuadradosPerfectos desde hasta
    | desde > hasta = 0
    | esCuadradoPerfecto desde = 1 + cantidadCuadradosPerfectos (desde+1) hasta
    | otherwise = cantidadCuadradosPerfectos (desde+1) hasta


--cantidadCuadradosPerfectos desde hasta = length (filter esCuadradoPerfecto [desde..hasta])

-- DISPERSION --

dispersion :: Number -> Number -> Number -> Number
dispersion med1 med2 med3 = mayorDeTres med1 med2 med3 - menorDeTres med1 med2 med3

dispersionChica :: Number -> Bool
dispersionChica dispersion = dispersion < 30

dispersionGrande :: Number -> Bool
dispersionGrande dispersion = dispersion > 100

menorDeTres :: Ord a => a -> a -> a -> a
menorDeTres elem1 elem2 elem3 = min elem1 (min elem2 elem3)

-- Ejercicios Dias --

diasParejos :: Number -> Number -> Number -> Bool
diasParejos med1 med2 med3 = dispersionChica (dispersion med1 med2 med3)

diasLocos :: Number -> Number -> Number -> Bool
diasLocos med1 med2 med3 = dispersionGrande (dispersion med1 med2 med3)

diasNormales :: Number -> Number -> Number -> Bool
diasNormales med1 med2 med3 = not (diasParejos med1 med2 med3) && not (diasLocos med1 med2 med3)


-- PINOS --

pesoPino :: Number -> Number
pesoPino altura = min altura 3 * 100 * 3 + max (altura - 3) 0 * 100 * 2

esPesoUtil :: Number -> Bool
esPesoUtil peso = peso >= 400 && peso <= 1000

sirvePino :: Number -> Bool
sirvePino altura = esPesoUtil (pesoPino altura)


-- MAS EJERCICIOS --

-- Pattern matching, caso XOR --
xor :: Bool -> Bool -> Bool
xor True True = False
xor bool1 bool2 = bool1 || bool2   -- Se comporta como un or logico sacando el primer caso


-- Secuencia de numeros de fibonacci --
fibonacci :: Number -> Number
fibonacci 0 = 0   -- Siempre para recursividad se necesita un caso base
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)  -- secuencia fibonacci: F(n) = F(n-1) + F(n-2)


-- Factorial de un numero --
factorial :: Number -> Number
factorial 0 = 1
factorial n
        | n > 0 = n * factorial (n-1)