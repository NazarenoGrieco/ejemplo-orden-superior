-- Recursividad
-- Fold
-- Data
-- Orden superior

-- Temática: Consumo sustancias / Fusión / Maximos

data Futbolista = UnFutbolista {
 experiencia::Int,
 sustancias::[String],
 doping::Bool} deriving Show
 
consumir:: String ->Futbolista->  Futbolista
consumir  nueva (UnFutbolista exp sus dop) = UnFutbolista (exp+1) (nueva:sus) (not dop) 

diego = UnFutbolista 10 [] False

maximo  [x] = x
maximo  (x:xs) = max x (maximo xs)

granFusion [x] = x
granFusion (x:xs) = fusionar x (granFusion xs)

granFusion' lista = ff fusionar lista
maximo' lista = ff max lista

ff criterio [x] = x 
ff criterio (x:xs) = criterio x (ff criterio xs )

ff2 criterio [x] = x 
ff2 criterio (x:xs) = ff2 criterio (criterio x (head xs):xs )


fusionar "vegeta" "goku"  = "gogeta"
fusionar  "goku" "vegeta" = "gogeta"
fusionar "legendario" "gogeta" = "messi"
fusionar  "gogeta" "legendario"= "messi"
fusionar  "maradona" "messi"= "dios"
fusionar   "messi" "maradona"= "dios"

--granFusion ["maradona","legendario","vegeta","goku"]
--"dios"


  



maximum' criterio [x] = x
maximum' criterio (x:xs) = max' criterio x (maximum' criterio xs)

max' criterio a b
 | criterio a > criterio b = a
 | otherwise = b

maximum'' criterio [x] = x
maximum'' criterio (x:xs) 
 | criterio x > criterio (head xs) = maximum'' criterio (x:tail xs)
 | otherwise = maximum'' criterio xs

potencia _ 0 = 1
potencia x y = x ^ y

head' (cabeza:cola) = cabeza

sum' [] = 0
sum' (x:xs) =  x + sum xs

ascendente [x] = True
ascendente (x:xs) = x < head xs && ascendente xs

f n = n:f (n*2)


