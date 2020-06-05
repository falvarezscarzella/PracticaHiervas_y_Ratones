module Lib where
import Text.Show.Functions

laVerdad = True

--Punto 1

type Enfermedades = [String] 

data Raton = UnRaton {
    nombre:: String,
    edad :: Float,
    peso:: Float,
    enfermedades :: Enfermedades
} deriving (Show)

cerebro = UnRaton "Cerebro" 9 0.2 ["Brucelosis", "Sarampion", "Tuberculosis"]
bicenterrata = UnRaton "Bicenterrata" 256 0.2 ["Sano"]
huesudo = UnRaton "Huesudo" 4 10 ["Obesidad", "Sinusitis"]

--Punto 2
type Hierba = Raton -> Raton

hierbaBuena :: Hierba
hierbaBuena raton = cambiarEdad (sqrt.edad $ raton) raton

cambiarEdad :: Float -> Raton -> Raton
cambiarEdad anios raton = raton{edad = anios }

hierbaVerde :: String -> Hierba
hierbaVerde terminacion raton = raton{enfermedades = filter (not.terminaCon terminacion) (enfermedades raton)}

terminaCon :: String -> String -> Bool
terminaCon terminacion enfermedad = (take (length terminacion).reverse) enfermedad == reverse terminacion

alcachofa :: Hierba
alcachofa raton = pierdePeso ((coefPerdidaDePeso raton)*(peso raton)) raton

pierdePeso :: Float -> Raton -> Raton
pierdePeso pesoAperder raton    |peso raton > pesoAperder = raton{peso = peso raton - pesoAperder }
                                |otherwise = raton{peso = 0 }

coefPerdidaDePeso :: Raton -> Float
coefPerdidaDePeso raton     | peso raton > 2 = 0.1
                            | otherwise = 0.05

hierbaZort :: Hierba
hierbaZort raton = cambiarNombre "Pinky" . cambiarEdad 0 . perderEnfermedades  $ raton

cambiarNombre :: String -> Raton -> Raton
cambiarNombre nombre raton = raton{nombre = nombre} 

perderEnfermedades :: Raton -> Raton
perderEnfermedades raton = raton{enfermedades = ["Sano"]}

hierbaDelDiablo :: Hierba
hierbaDelDiablo raton = pierdePeso 0.1 . eliminaEnfermedadesDeMasDeXLetras 10 $ raton

eliminaEnfermedadesDeMasDeXLetras :: Int -> Raton -> Raton
eliminaEnfermedadesDeMasDeXLetras numeroLetras raton= raton{enfermedades= filter ((numeroLetras<).length) (enfermedades raton)}    

--Punto 3

type Medicamento = Raton -> Raton

pondsAntiAge :: Medicamento
pondsAntiAge = hierbaBuena.hierbaBuena.hierbaBuena.alcachofa

reduceFatFast :: Int -> Medicamento
reduceFatFast potencia = hierbaVerde "Obesidad" . aplicarSegunPotencia alcachofa potencia 

aplicarMed ::  Hierba -> Raton -> Raton
aplicarMed hierba raton = hierba raton

aplicarSegunPotencia :: Hierba -> Int -> Raton -> Raton
aplicarSegunPotencia hierba potencia raton = foldr aplicarMed raton (replicate potencia hierba)

pdepCilina :: [String] -> Medicamento
pdepCilina sufijos =  aplicarListaDeHierbas (map hierbaVerde sufijos)

aplicarListaDeHierbas :: [Hierba] -> Raton -> Raton
aplicarListaDeHierbas listaHiervas raton = foldr aplicarMed raton listaHiervas

sufijosInfecciosas = [ "sis", "itis", "emia", "cocos"]    

--Punto 4

cantidadIdeal :: (Num a, Enum a) => (a -> Bool) -> a
cantidadIdeal condicion = head (filter condicion [1..])

lograrEstabilizar :: [Raton] -> Medicamento -> Bool
lograrEstabilizar ratones medicamento = (not.haySobrepeso) (map medicamento ratones) && ((<3).length) (map enfermedades ratones)

haySobrepeso :: [Raton] -> Bool
haySobrepeso ratones = any (>1) (map peso ratones)

potenciaIdeal :: [Raton] -> Int 
potenciaIdeal ratones = cantidadIdeal (\x -> lograrEstabilizar ratones (reduceFatFast x)) 

--Punto 5 

