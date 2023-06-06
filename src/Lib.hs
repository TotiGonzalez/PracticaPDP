type Enfermedad = [String]

data Valen = Raton {
    nombre :: String,
    edad :: Float,
    peso :: Float, 
    enfermedades :: Enfermedad 
} deriving Show

cerebro, bicenterrata, huesudo :: Valen

--------------PUNTO 1-------------------------

cerebro = Raton "Cerebro" 9 0.2 ["Brucelosis", "Sarampion", "Tuberculosis"]
bicenterrata = Raton "Bicenterrata" 256 0.2 []
huesudo = Raton "Huesudo" 4 10 ["AltaObesidad" , "Sinusitis"]

--------------PUNTO 2-------------------------

type Hierba = Valen -> Valen

hierbaBuena :: Hierba
hierbaBuena unRaton = rejuvenecer (sqrt (edad unRaton) ) unRaton 

rejuvenecer :: Float -> Valen -> Valen
rejuvenecer anios unRaton = unRaton { edad = anios }

hierbaVerde :: String -> Hierba
hierbaVerde terminacion unRaton = unRaton {enfermedades = filter (not.terminaCon terminacion) (enfermedades unRaton) }

terminaCon :: String -> String -> Bool
terminaCon terminacion unaEnfermedad = terminacion == drop (length unaEnfermedad - length terminacion) unaEnfermedad

alcachofa :: Hierba
alcachofa unRaton
    | (peso unRaton) > 2 = perderPeso (peso unRaton * 0.1) unRaton
    | otherwise          = perderPeso (peso unRaton * 0.05) unRaton

perderPeso :: Float -> Valen -> Valen
perderPeso pesoRatonRestar unRaton = unRaton {peso = max (peso unRaton - pesoRatonRestar) 0}

hierbaZort :: Hierba
hierbaZort = cambiarNombre "Pinky" . perderEnfermedades . rejuvenecer 0

cambiarNombre :: String -> Valen -> Valen
cambiarNombre nuevoNombre unRaton = unRaton {nombre = nuevoNombre}

perderEnfermedades :: Valen -> Valen
perderEnfermedades unRaton = unRaton {enfermedades = []}

hierbaDelDiablo :: Hierba
hierbaDelDiablo = perderPeso 0.1 . eliminarEnfermedades 10

eliminarEnfermedades :: Int -> Valen -> Valen
eliminarEnfermedades cantLetrasMax unRaton = unRaton { enfermedades = filter ((<=cantLetrasMax) . length) (enfermedades unRaton) }

--------------PUNTO 3-------------------------

type Medicamento = Valen -> Valen

pondsAntiAge :: Medicamento
pondsAntiAge = hacerMedicamento [ hierbaBuena , hierbaBuena , hierbaBuena , alcachofa]

hacerMedicamento :: [Hierba] -> Medicamento
hacerMedicamento unasHierbas = foldl1 (.) unasHierbas
hacerMedicamento [] = id
hacerMedicamento (x:xs) = x . hacerMedicamento xs

reduceFatFast :: Int -> Medicamento
reduceFatFast cantPotencia = hacerMedicamento [replicate cantPotencia alcachofa, hierbaVerde "Obesidad"]

pdepCilina :: Medicamento
pdepCilina = hacerMedicamento (map hierbaVerde sufijosInfecciosas)  

sufijosInfecciosas :: [String]
sufijosInfecciosas = [ "sis", "itis", "emia", "cocos"]

--------------PUNTO 4-------------------------

cantidadIdeal :: Num a => (a -> Bool) -> a
cantidadIdeal condicion = filter (condicion [1..])
