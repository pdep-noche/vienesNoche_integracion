module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Animal= Raton {nombre :: String, edad :: Number, peso :: Number, enfermedades :: [String]} deriving Show
-- Ejemplo de raton
cerebro = Raton "Cerebro" 9.0 0.2 ["brucelosis", "sarampión", "tuberculosis"]
-- Estos son las enfermedades infecciosas
enfermedadesInfecciosas = [ "brucelosis", "tuberculosis"]

modificarNombre :: (String -> String) -> Animal -> Animal
modificarNombre f raton = raton {nombre = (f.nombre) raton}

modificarEdad :: (Number -> Number) -> Animal -> Animal
modificarEdad f raton = raton {edad = (f.edad) raton}

modificarPeso :: ( Number -> Number) -> Animal -> Animal
modificarPeso f raton = raton { peso = (f.peso) raton}

modificarEnfermedades :: ([String] -> [String]) -> Animal ->Animal
modificarEnfermedades f raton = raton {enfermedades = (f.enfermedades) raton}

hierbaBuena ::  Animal -> Animal
hierbaBuena raton = modificarEdad sqrt raton

hierbaVerde :: String -> Animal -> Animal
hierbaVerde enfermedad raton = modificarEnfermedades (filter (/= enfermedad))  raton

alcachofa :: Animal -> Animal
alcachofa raton = modificarPeso  bajarPeso   raton

bajarPeso :: Number -> Number
bajarPeso peso | peso > 2 = peso * 0.9
              | otherwise = peso * 0.95


hierbaMagica :: Animal -> Animal
hierbaMagica raton = (modificarEdad (*0). modificarEnfermedades (const [])) raton


medicamentos :: [(Animal -> Animal)] -> Animal -> Animal
medicamentos hierbas raton = foldl (\unRaton unaHierba -> unaHierba unRaton ) raton   hierbas

medicamentos' hierbas raton = foldl (flip ($)) raton hierbas

antiAge :: Animal -> Animal
antiAge raton = medicamentos (replicate 3 hierbaBuena ++ [alcachofa])  raton

reduceFatFast :: Number -> Animal -> Animal
reduceFatFast potencia raton = medicamentos ([hierbaVerde "obesidad"] ++ replicate potencia alcachofa) raton

hierbaMilagrosa :: Animal -> Animal
hierbaMilagrosa raton = medicamentos (map hierbaVerde enfermedadesInfecciosas) raton

cantidadIdeal :: (Number -> Bool) -> Number
cantidadIdeal f =  head .filter f  $ [1..]

estanMejoresQueNunca :: [Animal] -> (Animal -> Animal) -> Bool
estanMejoresQueNunca ratones unMedicamento = all ((<1).peso.unMedicamento) ratones

{-
*Spec> estanMejoresQueNunca [cerebro] hierbaMilagrosa
True
-}

experimento :: [Animal] -> Number
experimento ratones = cantidadIdeal(estanMejoresQueNunca ratones.reduceFatFast) 

{-*Spec> experimento [cerebro] 
1
-}

data Postulante = UnPostulante {nombrePostulante :: String, edadPostulante :: Number, remuneracion :: Number, conocimientos :: [String]} deriving Show 
pepe = UnPostulante "Jose Perez" 35 15000.0 ["Haskell", "Prolog", "Wollok", "C"]
tito = UnPostulante "Roberto Gonzalez" 20 12000.0 ["Haskell", "Php"]

type Nombre = String
data Puesto = UnPuesto {puesto:: String, conocimientoRequeridos :: [String]} deriving Show
jefe = UnPuesto "gerente de sistemas" ["Haskell", "Prolog", "Wollok"]
chePibe = UnPuesto "cadete" ["ir al banco"]
 
apellidoDueno:: Nombre
apellidoDueno = "Gonzalez"

type Requisito = Postulante -> Bool

tieneConocimientos :: Puesto -> Requisito
tieneConocimientos puesto unPostulante = all (\conociRequ -> elem conociRequ (conocimientos unPostulante)). conocimientoRequeridos $ puesto

{-*Spec Library Spec> tieneConocimientos jefe pepe 
True
*Spec Library Spec> tieneConocimientos jefe tito
False
-}

edadAceptable :: Number -> Number -> Requisito
edadAceptable edadMin edadMax postulante = edadPostulante postulante >= edadMin && edadPostulante postulante <= edadMax

{-
*Spec Library Spec> edadAceptable 32 45 pepe
True
*Spec Library Spec> edadAceptable 32 40 tito
False
-}

sinArreglo:: Requisito 
sinArreglo postulante =  (/= apellidoDueno).last.words.nombrePostulante $ postulante

{-
*Spec Library Spec> sinArreglo tito
False
*Spec Library Spec> sinArreglo pepe
True
-}

--2
preseleccion :: [Postulante] -> [Requisito] -> [Postulante]
preseleccion postulantes requisitos = filter (cumpleTodos requisitos) postulantes 

cumpleTodos :: [Requisito] -> Postulante -> Bool
cumpleTodos requisitos postulante = all ($ postulante) requisitos


{-
preseleccion [pepe, tito] [edadAceptable 30 40, tieneConocimientos  jefe, sinArreglo ]
[UnPostulante {nombrePostulante = "Jose Perez", edadPostulante = 35, remuneracion = 15000, conocimientos = ["Haskell","Prolog","Wollok","C"]}]
-}
