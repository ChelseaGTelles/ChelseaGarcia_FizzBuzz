module Soluciones (sumarLista, factorial, numerosPares, longitudCadena, reversoLista, duplicarElementos, filtrarPares, fibonacci, divisores, esPalindromo, main) where

-- 1
sumarLista :: [Int] -> Int
sumarLista = sum

-- 2
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- 3
numerosPares :: Int -> [Int]
numerosPares n = [x | x <- [0..n], even x]

-- 4
longitudCadena :: String -> Int
longitudCadena = length

-- 5
reversoLista :: [a] -> [a]
reversoLista = reverse

-- 6
duplicarElementos :: [Int] -> [Int]
duplicarElementos = concatMap (\x -> [x, x])

-- 7
filtrarPares :: [Int] -> [Int]
filtrarPares = filter even

-- 8
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- 9
divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], n `mod` x == 0]

-- 10
esPalindromo :: String -> Bool
esPalindromo str = str == reverse str

main :: IO ()
main = do
    print (sumarLista [1, 2, 3, 4])      
    print (factorial 5)                  
    print (factorial 8)                  
    print (numerosPares 10)              
    print (numerosPares 21)                
    print (longitudCadena "Hola")       
    print (longitudCadena "ESTAESUNACADENAHOLA")  
    print (reversoLista [1, 2, 3])         
    print (reversoLista [4, 12, 43])       
    print (duplicarElementos [1, 2, 3]) 
    print (duplicarElementos [1, 2, 3, 4, 5, 6, 3])    
    print (filtrarPares [1, 2, 3, 4])      
    print (fibonacci 6)                   
    print (divisores 10) 
    print (divisores 5)                  
    print (esPalindromo "LAAL")        
    print (esPalindromo "hello")          
