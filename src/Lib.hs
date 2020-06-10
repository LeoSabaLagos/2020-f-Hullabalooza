module Lib where
import Text.Show.Functions

laVerdad = True

---------------------------------------------------------------------------------------------
-- Punto 1

-- Festival

type Lugar = String
type Publico = Float 
type Animo = String

data Festival = Festival{
    lugar :: Lugar,
    publico :: Publico,
    animoFestival :: Animo,
    bandas :: [Banda]
}deriving(Show)

type Descripcion = String
type Decibel = Int

data Banda = Banda{
    descripciones :: [Descripcion],
    decibeles :: Decibel,
    genero :: Genero
}deriving(Show)

type Genero = Festival -> Festival

rockNacional :: Genero 
rockNacional = aumentarPublico 100

aumentarPublico :: Publico -> Festival -> Festival
aumentarPublico aumento festival = festival{publico = publico festival + aumento} 

pop :: Genero
pop festival
    | animoIndiferente festival = 
        ((cambiarAnimo "euforico").(aumentarPublico (publico festival))) festival
    | otherwise = festival 

animoIndiferente :: Festival -> Bool
animoIndiferente = (=="indiferente").animoFestival

cambiarAnimo :: Animo -> Festival -> Festival
cambiarAnimo animo festival = festival{animoFestival = animo}

heavyMetal :: Genero
heavyMetal = metal "pesado"

trashMetal :: Genero
trashMetal = metal "basura" 

metal :: Animo -> Genero
metal animo festival = 
    ((agregarTipoMetal animo).(aumentarPublico (unoPorCiento festival))) festival

unoPorCiento :: Festival -> Float
unoPorCiento = multiplicarPorCeroPuntoUno.publico

multiplicarPorCeroPuntoUno :: Float -> Float
multiplicarPorCeroPuntoUno =  fromIntegral.ceiling.(*0.1)

agregarTipoMetal :: Animo -> Festival -> Festival
agregarTipoMetal animo festival = festival{animoFestival = (animoFestival festival) ++ espacio ++ animo} 

espacio :: String
espacio = " "

-- Tocar
tocar :: Festival -> Banda ->  Festival
tocar festival banda = genero banda festival
---------------------------------------------------------------------------------------------
-- Punto 2

-- Festivales de ejemplo
cosquin = Festival "Cordoba" 500 "normal" [soda]
hulla = Festival "Springfield" 1000 "indiferente" [miranda,losRedondos,metallica,soda]

hullapopu = Festival "Springfield" 1000 "indiferente" [metallica,miranda,losRedondos,soda]

-- Bandas de ejemplo
losRedondos = Banda ["legendaria","pogosa"] 45 rockNacional
soda = Banda ["irrepetible"] 40 rockNacional
miranda = Banda ["insipida","incolora","inodora"] 60 pop
metallica = Banda ["legendaria","vendida"] 60 heavyMetal
slipknot = Banda ["ruidosa"] 70 trashMetal

---------------------------------------------------------------------------------------------
-- Punto 3
theStrokes = Banda ["suicidio asistido","emocional","linda"] 45 (pop.heavyMetal)
---------------------------------------------------------------------------------------------
-- Punto 4

suceder :: Festival -> Festival 
--suceder festival = foldr tocar festival (bandas festival)
suceder festival = foldl tocar festival (bandas festival)
---------------------------------------------------------------------------------------------
-- Punto 5
type Clasificacion = Banda -> Bool

vendida :: Clasificacion
vendida banda 
    | tresOMasDescripciones banda || tieneDescripcion "vendida" banda = True
    | otherwise = False

tresOMasDescripciones :: Banda -> Bool
tresOMasDescripciones = (>=3).length.descripciones

acustica :: Clasificacion
acustica = masDeCiertosDecibeles 55

masDeCiertosDecibeles :: Int -> Banda -> Bool
masDeCiertosDecibeles minDecibeles = (>minDecibeles).decibeles

legendaria :: Clasificacion
legendaria banda = tieneDescripcion "legendaria" banda && masDeCiertosDecibeles 40 banda


tieneDescripcion :: Descripcion -> Banda -> Bool
tieneDescripcion descripcionBuscada = (elem descripcionBuscada).descripciones
---------------------------------------------------------------------------------------------
-- Punto 6

type Puntos = Int
type Popularidad = Int

popularidad :: Banda -> [Clasificacion] -> Popularidad
popularidad banda clasificaciones = (obtenerPuntos.(obtenerClasificaciones clasificaciones)) banda 

obtenerClasificaciones :: [Clasificacion] -> Banda -> [Bool]
obtenerClasificaciones clasificaciones banda  = map ($banda) clasificaciones

obtenerPuntos :: [Bool] -> Popularidad
obtenerPuntos [] = 0
obtenerPuntos (cabeza : cola) 
    | cabeza = 100 + obtenerPuntos cola
    | otherwise = obtenerPuntos cola
---------------------------------------------------------------------------------------------
-- Punto 7
buenFest :: Festival -> Bool
buenFest festival = popularidadCronologica festival && popularidadMayorAMil festival

popularidadCronologica :: Festival -> Bool
popularidadCronologica = menorAMayorCronologicamente.obtenerPopularidades

menorAMayorCronologicamente :: [Popularidad] -> Bool
menorAMayorCronologicamente [popu] = True
menorAMayorCronologicamente (popu1 : popu2 : colaPopus)
    | popu1 > popu2 = True && menorAMayorCronologicamente (popu2 : colaPopus)
    | otherwise = False

obtenerPopularidades :: Festival -> [Popularidad]
obtenerPopularidades = (obtenerListaPopularidades clasificacionesPosibles).bandas

obtenerListaPopularidades ::  [Clasificacion] -> [Banda] -> [Popularidad]
obtenerListaPopularidades clasificacionesPosibles = map (`popularidad` clasificacionesPosibles)
    
clasificacionesPosibles :: [Clasificacion]
clasificacionesPosibles = [vendida,acustica,legendaria]

popularidadMayorAMil :: Festival -> Bool
popularidadMayorAMil = (>1000).sum.obtenerPopularidades
---------------------------------------------------------------------------------------------
-- Punto 8
-- un quilombo marcar en el codigo
---------------------------------------------------------------------------------------------
-- Punto 9
-- Las listas de bandas de un festival no
-- ejemplo: si queremos calcular la lista de popularidades de las bandas de un festival
-- no termina nunca

-- y la de posibles clasificaciones para evaluar la popularidad tampoco, ya que nunca 
-- dejaria de evaluar la banda con las distintos criterios de clasificacion posibles

-- Todo esto es por el lazy evaluation que utiliza Haskell
---------------------------------------------------------------------------------------------
