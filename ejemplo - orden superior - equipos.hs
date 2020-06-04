
data Equipo = Equipo {
    nombre::String,
    jugadores:: [String],
    puntos::Int,
    dt::String
} deriving Show


equiposDeEjemplo = [boca, river, racing]

boca = Equipo  "Boca Juniors" ["palermo","riquelme"] 10 "bianchi"
river = Equipo  "River Plate" ["quinteros","martinez","prato"] 10 "gallardo"
racing = Equipo "Racing Club" [] 7 "merlo"


ganaPartido :: Equipo -> Equipo
ganaPartido equipo = equipo{puntos= puntos equipo + 3 }

perderPartido :: Equipo -> Equipo
perderPartido equipo = equipo

contratarJugador:: String -> Equipo -> Equipo
contratarJugador jug equipo = equipo{jugadores =  jug:jugadores equipo}

cotizacion equipo = puntos equipo + length (jugadores equipo)

premios equipo = puntos equipo * 3


esBueno equipo = puntos equipo > 8

esGenial equipo = elem "riquelme" (jugadores equipo)

f x y = filter (\e->e >y)  [1..x]
-- Requerimiento
-- cuantosEquiposBuenosHay equiposDeEjemplo  --> 2

cuantosEquiposBuenosHay [] = 0
cuantosEquiposBuenosHay (e:es) 
  | esBueno e = 1 + cuantosEquiposBuenosHay es
  | otherwise =  cuantosEquiposBuenosHay es

cuantosEquiposGenialesHay [] = 0
cuantosEquiposGenialesHay (e:es) 
  | esGenial e = 1 + cuantosEquiposGenialesHay es
  | otherwise =  cuantosEquiposGenialesHay es


cuantosEquiposSon:: (t1 -> Bool) -> [t1] -> Int
cuantosEquiposSon algo [] = 0
cuantosEquiposSon algo (e:es) 
  | algo e = 1 + cuantosEquiposSon algo es
  | otherwise =  cuantosEquiposSon algo es


a1 b c d = b c + b d

a2 b c d 
   | b d > c d = c
   | otherwise = b

a3 b c = b c

a4 b c d = b d c

a5 b c d = b (c d)

mayorQueDos x = x > 2
--f17 x = x * x + 17













cuantosEquiposBuenosHay' equipos = length  ( equiposBuenos esBueno equipos)

equiposBuenos criterio [] = []
equiposBuenos criterio (e:es) 
  | criterio e = e:equiposBuenos criterio es
  | otherwise = equiposBuenos criterio es

