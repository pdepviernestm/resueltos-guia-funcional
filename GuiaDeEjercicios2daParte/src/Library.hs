module Library where
import PdePreludat


-- EJERCICIO PESCADOR --


data Pescador = UnPescador {
    apodo :: String,
    capturas :: [Pescado],
    pesoPescador :: Number
}deriving(Show, Eq)

data Pescado = UnPescado {
    especiePez :: String,
    pesoPescado :: Number
}deriving(Show, Eq)


-- 1)
cantidadDePiezasPescadas :: Pescador -> Number
cantidadDePiezasPescadas = length . capturas


-- 2)
mejoroLaCaptura :: Pescador -> Bool
mejoroLaCaptura pescador = (pesoPescado . last . capturas) pescador > (pesoPescado . head . capturas) pescador


-- 3)
nuevaCaptura :: Pescador -> Pescado -> Pescador
nuevaCaptura pescador pescado = pescador{capturas = capturas pescador ++ [pescado]}


-- 4)
saleMerienda :: Pescador -> Pescado -> Pescador
saleMerienda pescador pescado = pescador{pesoPescador = pesoPescador pescador + pesoPescado pescado * 0.2}


-- 5)
desecharPescados :: Pescador -> Pescador
desecharPescados pescador = pescador{capturas = []}



-- EJERCICIO FUENTE DE LOS DESEOS --

data Persona = UnaPersona {
    edad :: Number,
    nombre :: String,
    felicidonios :: Number,
    suenios :: [String]
}deriving(Show, Eq)


-- 1)
coeficienteDeSatisfaccionEs :: Persona -> Number
coeficienteDeSatisfaccionEs (UnaPersona edadPersona _ felicidoniosPersona sueniosPersona)
                        | felicidoniosPersona > 100 = felicidoniosPersona * edadPersona
                        | felicidoniosPersona <= 100 && felicidoniosPersona > 50 = felicidoniosPersona * length sueniosPersona
                        | otherwise = 40


-- 2)
tieneNombreLargo :: Persona -> Bool
tieneNombreLargo = (> 10) . length . nombre


-- 3)
esSuertuda :: Persona -> Bool
esSuertuda = even . (* 3) . coeficienteDeSatisfaccionEs


-- 4)
tieneNombreLindo :: Persona -> Bool
tieneNombreLindo persona = head (nombre persona) == 'L'


-- 5)
sueniosCumplidos :: Persona -> Persona
sueniosCumplidos persona = persona{felicidonios = felicidonios persona + coeficienteDeSatisfaccionEs persona * (length . suenios) persona, suenios = []}


fuenteDeLosDeseos :: Persona -> Persona
fuenteDeLosDeseos persona
                | esSuertuda persona = sueniosCumplidos persona{felicidonios = felicidonios persona + 10}
                | otherwise = persona{felicidonios = felicidonios persona + 10}





-- EJERCICIO Pinky y Cerebro --

data Animal = UnAnimal {
    coeficiente :: Number,
    especie :: String,
    capacidades :: [String]
}deriving(Show, Eq)

-- 1)

cerebro :: Animal
cerebro = UnAnimal 200 "Raton" ["Estratega", "Cientifico", "Liderazgo", "Gaenio"]

pinky :: Animal
pinky = UnAnimal 120 "Raton" ["Comico", "Leal"]

-- 2)
inteligenciaSuperior :: Number -> Animal -> Animal
inteligenciaSuperior n animal = animal{coeficiente = coeficiente animal + n}

pinkificado :: Animal -> Animal
pinkificado animal = animal{capacidades = []}

conSuperpoderes :: Animal -> Animal
conSuperpoderes animal
            | especie animal == "Elefante" = animal{capacidades = capacidades animal ++ ["no tenerle miedo a los ratones"]}
            | especie animal == "Raton" && coeficiente animal > 100 = animal{capacidades = capacidades animal ++ ["Hablar"]}
            | otherwise = animal

-- 3)
esAntropomorfico :: Animal -> Bool
esAntropomorfico animal = (elem "Hablar" . capacidades) animal && coeficiente animal > 60

noTanCuerdo :: Animal -> Bool
noTanCuerdo animal = length (capacidadesPinkiescas (capacidades animal)) > 2

capacidadesPinkiescas :: [String] -> [String]
capacidadesPinkiescas = filter esPinkieska

esPinkieska :: String -> Bool  --No me convence esta solucion
esPinkieska capacidad = capacidad == "Hacer " ++ drop 5 capacidad && length (drop 5 capacidad) <= 4 && any esVocal (drop 5 capacidad)

esVocal :: Char -> Bool
esVocal c = c `elem` "aeiouAEIOU"


-- 4)
esAntropomorficoLuegoDeExperimento :: Number -> Animal -> Bool
esAntropomorficoLuegoDeExperimento n animal = (not . esAntropomorfico) animal && (esAntropomorfico . inteligenciaSuperior n) animal
