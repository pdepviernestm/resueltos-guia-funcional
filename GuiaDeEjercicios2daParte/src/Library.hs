module Library where
import PdePreludat

data Pescador = UnPescador {
    apodo :: String,
    capturas :: [Pescado],
    pesoPescador :: Number
}deriving(Show, Eq)

data Pescado = UnPescado {
    especie :: String,
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