import Data.Char (toUpper)
import qualified Data.Map as Map
import Data.Map (Map)

-- 1
descuento :: Double -> Double -> Double
descuento precio porcentaje = precio * (1 - porcentaje / 100)

cIVA :: Double -> Double -> Double
cIVA precio iva = precio * (1 + iva / 100)

precioFinal :: [(Double, Double)] -> (Double -> Double -> Double) -> Double
precioFinal cesta funcion = sum [funcion precio porcentaje | (precio, porcentaje) <- cesta]

-- 2
aplicarALista :: (a -> b) -> [a] -> [b]
aplicarALista f lista = map f lista

-- 3
longitudPalabras :: String -> Map String Int
longitudPalabras frase = Map.fromList [(palabra, length palabra) | palabra <- words frase]

-- 4
clasificarNotas :: Map String Int -> Map String String
clasificarNotas notas = Map.fromList [(map toUpper asignatura, clasificar n) | (asignatura, n) <- Map.toList notas]
  where
    clasificar n
      | n >= 95 = "Excelente"
      | n >= 85 = "Notable"
      | n >= 75 = "Bueno"
      | n >= 70 = "Suficiente"
      | otherwise = "Desempeño insuficiente"

-- 5
moduloVector :: [Double] -> Double
moduloVector v = sqrt $ sum $ map (^2) v

-- 6
media :: [Double] -> Double
media lista = sum lista / fromIntegral (length lista)

desvEstandar :: [Double] -> Double
desvEstandar lista = sqrt (sum [(x - m)^2 | x <- lista] / fromIntegral (length lista))
  where m = media lista

valoresAtipicos :: [Double] -> [Double]
valoresAtipicos lista = [x | x <- lista, abs ((x - m) / dt) > 2]  
  where
    m = media lista
    dt = desvEstandar lista

main :: IO ()
main = do
    print $ descuento 100 20
    print $ cIVA 100 21
    let cesta1 = [(100, 20), (100, 21)] 
    print $ precioFinal cesta1 descuento
    print $ precioFinal cesta1 cIVA 

    print $ aplicarALista (*2) [1,2,3,4]
    
    print $ longitudPalabras "Esta es mi tarea de ejercicios" 
    
    let notas = Map.fromList [("Matematicas", 95), ("Historia", 80), ("Fisica", 70)]
    print $ clasificarNotas notas 
    
    print $ moduloVector [3,4]

    let muestra = [1, 2, 3, 4, 5, 100]
    print $ valoresAtipicos muestra