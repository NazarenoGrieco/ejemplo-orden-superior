-- LO QUE YA SABEMOS
-- Podemos tener tres (o más) funciones así:
suma1aTodos :: [Int] -> [Int]
suma1aTodos [] = []    
suma1aTodos (x:xs) = x + 1: suma1aTodos xs

suma5aTodos :: [Int] -> [Int]
suma5aTodos [] = []    
suma5aTodos (x:xs) = x + 5: suma5aTodos xs

suma10aTodos :: [Int] -> [Int]
suma10aTodos [] = []    
suma10aTodos (x:xs) = x + 10: suma5aTodos xs

-- pero las podemos reemplazar por una sola, 
-- con un parámetro con el número a sumar

sumaNaTodos :: Int -> [Int] -> [Int]
sumaNaTodos n [] = []    
sumaNaTodos n (x:xs) = x + n: sumaNaTodos n xs

-- Y si de todas maneras necesitamos respetar el tipo de las funciones anteriores, 
-- podemos hacer 
-- suma5aTodos :: [Int] -> [Int]
-- suma5aTodos lista = sumaNaTodos 5 lista

-- PARA QUE USAR ORDEN SUPERIOR
-- Podemos tener 3 (o más) varias funciones que son más diferentes
-- pero que siguen teniendo características comunes

suma1aTodos' :: [Int] -> [Int]
suma1aTodos' [] = []    
suma1aTodos' (x:xs) = x + 1: suma1aTodos' xs

duplicaTodos :: [Int] -> [Int]
duplicaTodos [] = []
duplicaTodos (x:xs) = x * 2: duplicaTodos xs

restaTodos :: [Int] -> [Int]
restaTodos [] = []
restaTodos (x:xs) = x - (div x 3): restaTodos xs

-- La diferencia está en la tarea que se hace con los elementos 
-- pero se mantiene la idea de que se la hace con todos los elementos de la lista

haceAlgoConTodos::(Int->Int) -> [Int]-> [Int]
haceAlgoConTodos f [] = []
haceAlgoConTodos f (x:xs) = f x :haceAlgoConTodos f xs

suma1 x = x + 1

duplica x = x*2

resta x = x - (div x 3) 

--Ejemplo de uso
-- haceAlgoConTodos suma1 [1,2,3,4]
-- haceAlgoConTodos duplica [1,2,3,4]
-- haceAlgoConTodos resta [1,2,3,4]

funcionLoca::Int->Int
funcionLoca x = (2 + 3*x + 2*x*x)





