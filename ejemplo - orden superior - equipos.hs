comoSeLlama :: Equipo -> String
comoSeLlama (Equipo nom _ _ _ ) = nom

nombrePersona ::  (String, Int, Bool, Float) -> String
nombrePersona (nombre,_,_,_)= nombre


data Equipo = Equipo {
    nombre::String,
    jugadores:: [String],
    puntos::Int,
    dt::String
} deriving Show


equiposDeEjemplo :: [Equipo]
equiposDeEjemplo = [boca, river, racing]


boca,river,racing::Equipo
boca = Equipo  "Boca Juniors" ["palermo","riquelme"] 10 "bianchi"
river = Equipo  "River Plate" ["quinteros","martinez","prato"] 10 "gallardo"
racing = Equipo "Racing Club" [] 7 "merlo"


ganaPartido :: Equipo -> Equipo
--ganaPartido equipo = equipo{puntos= puntos equipo + 3 }
ganaPartido (Equipo nom jugadores puntos director) = 
  Equipo nom jugadores (puntos + 3)  director

perderPartido :: Equipo -> Equipo
perderPartido equipo = equipo

contratarJugador:: String -> Equipo -> Equipo
--contratarJugador jug equipo = equipo{jugadores =  jug:jugadores equipo}
contratarJugador jug (Equipo nom jugadores puntos director) = 
  Equipo nom (jug:jugadores) puntos director

desastre:: Equipo -> Equipo
desastre (Equipo nom jugadores puntos director) = 
  Equipo ("chau " ++ nom) []  (puntos - 17) director


cotizacion::Equipo -> Int
cotizacion equipo = puntos equipo + length (jugadores equipo)
--cotizacion (Equipo nom jugs pts dirt) = pts + length jugs

premios:: Equipo -> Int
premios equipo = puntos equipo * 3

esBueno::Equipo -> Bool
esBueno equipo = puntos equipo > 8

esGenial::Equipo -> Bool
esGenial equipo = elem "riquelme" (jugadores equipo)

loTieneA :: String -> Equipo -> Bool
loTieneA persona equipo = elem persona (jugadores equipo) || dt equipo == persona

mejor:: (Equipo -> Int )   -> Equipo  -> Equipo -> Bool
mejor criterio equipo1 equipo2 = criterio equipo1 > criterio equipo2


--masPuntos ::Equipo -> Equipo -> Bool
--masPuntos equipo1 equipo2 =  puntos equipo1 > puntos equipo2

--esMejorCotizado:: Equipo -> Equipo -> Bool
--esMejorCotizado equipo1 equipo2 = cotizacion equipo1 > cotizacion equipo2

--directorTecnicoMasExperimentado:: Equipo -> Equipo -> Bool
--directorTecnicoMasExperimentado e1 e2 = experiencia (dt e1) > experiencia (dt e2)

experienciaDelTecnico equipo = experiencia (dt equipo)


experiencia nombre = length nombre * 2
-- Requerimiento
-- cuantosEquiposBuenosHay equiposDeEjemplo  --> 2

cuantosEquiposBuenosHay :: [Equipo] -> Int
cuantosEquiposBuenosHay [] = 0
cuantosEquiposBuenosHay (e:es) 
  | esBueno e = 1 + cuantosEquiposBuenosHay es
  | otherwise =  cuantosEquiposBuenosHay es

cuantosEquiposGenialesHay :: [Equipo] -> Int
cuantosEquiposGenialesHay [] = 0
cuantosEquiposGenialesHay (e:es) 
  | esGenial e = 1 + cuantosEquiposGenialesHay es
  | otherwise =  cuantosEquiposGenialesHay es


cuantosEquiposSon ::(Equipo ->  Bool )  ->[Equipo] -> Int
cuantosEquiposSon algo [] = 0
cuantosEquiposSon algo (e:es) 
  | algo e = 1 + cuantosEquiposSon algo es
  | otherwise =  cuantosEquiposSon algo es


a1 b c d = b c + b d

a2 b c d 
   | b d > c d = c
   | otherwise = b
-- un comentario
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

