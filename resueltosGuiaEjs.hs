
-- Devolver el signo de un numero --
signo :: Float -> Char
signo numero
        | numero > 0 = '+'
        | numero < 0 = '-'
        | otherwise = '0'


-- Recibir cadena de clima y devolver probabilidad --
probabilidadDeLluvia :: String -> Int
probabilidadDeLluvia "Despejado" = 0
probabilidadDeLluvia "Nublado" = 25
probabilidadDeLluvia "Lluvioso" = 100
probabilidadDeLluvia _ = 50


-- Dados 3 elementos, devolver el mayor de los 3 --
mayorDeTres :: Ord a => a -> a -> a -> a       -- indico que el tipo a pertenece a la clase Ord, lo que me habilita a usar operadores de comparacion como >=, <=, > y <
mayorDeTres elem1 elem2 elem3
                    | elem1 >= elem2 = max elem1 elem3
                    | otherwise = max elem2 elem3


-- Que año es bisiesto --
bisiesto :: Int -> Bool
bisiesto anio = (anio `mod` 4 == 0 && anio `mod` 100 /= 0) || anio `mod` 400 == 0


-- Saber si un numero es multiplo de 3 --
esMultiploDeTres :: Int -> Bool
esMultiploDeTres numero = numero `mod` 3 == 0

-- Saber si un numero es multiplo de otro --
esMultiploDe :: Int -> Int -> Bool
esMultiploDe dividendo divisor = dividendo `mod` divisor == 0



-- Calcular Area de un rectangulo --
areaRectangulo :: Float -> Float -> Float
areaRectangulo base altura = base * altura

-- Volumen de un cubo --
volumenCubo :: Float -> Float -> Float -> Float
volumenCubo base altura profundidad = base * altura * profundidad


-- Calcular las superficies de cada color (Guia) --
-- Cuadrado, circulo, cuadrado menor --

areaCuadrado :: Float -> Float
areaCuadrado x = x * x
-- areaCuadrado x = areaRectangulo x x

areaCirculo :: Float -> Float
areaCirculo x = pi * (x ^ 2)

areaCuadradoMenor :: Float -> Float
areaCuadradoMenor x = areaCuadrado (sqrt ((x/2)^2 + (x/2)^2))  -- Pitagoras


-- Conversion de temperaturas --
celsiusToFahr :: Float -> Float
celsiusToFahr celsius = (9/5) * celsius + 32

fahrToCelsius :: Float -> Float
fahrToCelsius fahr = (fahr - 32) * (5/9)

haceFrioF :: Float -> Bool
haceFrioF fahr = fahrToCelsius fahr < 8


-- Un número entero es cuadrado perfecto, si es el cuadrado de algún número entero. --
esCuadradoPerfecto :: Int -> Bool
esCuadradoPerfecto numero = (floor (sqrt (fromIntegral numero)) ^ 2) == numero   -- fromIntegral convierte un numero entero en flotante, en este caso para poder calcular la raiz

cuadradoPerfectoMayorQue :: Int -> Int
cuadradoPerfectoMayorQue numero
                    | esCuadradoPerfecto (numero+1) = numero + 1
                    | otherwise = cuadradoPerfectoMayorQue (numero+1)   -- Recursividad

cantidadCuadradosPerfectos :: Int -> Int -> Int
cantidadCuadradosPerfectos desde hasta
                                | desde > hasta = 0
                                | esCuadradoPerfecto desde = 1 + cantidadCuadradosPerfectos (desde+1) hasta
                                | otherwise = cantidadCuadradosPerfectos (desde+1) hasta


--cantidadCuadradosPerfectos desde hasta = length (filter esCuadradoPerfecto [desde..hasta])


-- Minimo comun multiplo



-- maximo comun divisor



-- DISPERSION --

dispersion :: Int -> Int -> Int -> Int
dispersion med1 med2 med3 = mayorDeTres med1 med2 med3 - menorDeTres med1 med2 med3

dispersionChica :: Int -> Bool
dispersionChica dispersion = dispersion < 30

dispersionGrande :: Int -> Bool
dispersionGrande dispersion = dispersion > 100

menorDeTres :: Ord a => a -> a -> a -> a
menorDeTres elem1 elem2 elem3
                    | elem1 <= elem2 = min elem1 elem3
                    | otherwise = min elem2 elem3

diasParejos :: Int -> Int -> Int -> Bool
diasParejos med1 med2 med3 = dispersionChica (dispersion med1 med2 med3)

diasLocos :: Int -> Int -> Int -> Bool
diasLocos med1 med2 med3 = dispersionGrande (dispersion med1 med2 med3)

diasNormales :: Int -> Int -> Int -> Bool
diasNormales med1 med2 med3 = not (diasParejos med1 med2 med3) && not (diasLocos med1 med2 med3)


-- PINOS --

pesoPino :: Int -> Int
pesoPino altura = min altura 3 * 100 * 3 + max (altura - 3) 0 * 100 * 2

esPesoUtil :: Int -> Bool
esPesoUtil peso = peso `elem` [400..1000]   -- peso >= 400 && peso <= 1000

sirvePino :: Int -> Bool
sirvePino altura = esPesoUtil (pesoPino altura)

