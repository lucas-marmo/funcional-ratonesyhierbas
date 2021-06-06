module Library where
import PdePreludat

data Raton = UnRaton {
    nombre :: String
,   edad :: Number
,   peso :: Number
,   enfermedades :: [Enfermedad]
} deriving (Show,Eq)

type Enfermedad = String

{---------------------------PUNTO 1---------------------------}
cerebro = UnRaton "Cerebro" 9 0.2 ["Brucelosis","Sarampion","Tuberculosis"]
bicenterrata = UnRaton "Bicenterrata" 256 0.2 []
huesudo = UnRaton "Huesudo" 4 10 ["Obesidad","Sinusitis"]

{---------------------------PUNTO 2---------------------------}
type Hierba = Raton -> Raton

ingerirHierba :: Raton -> Hierba -> Raton
ingerirHierba raton hierba = hierba raton

hierbaBuena :: Hierba
hierbaBuena raton = raton {edad = sqrt (edad raton)}

hierbaVerde :: String -> Hierba
hierbaVerde finCadena raton = raton {enfermedades = eliminarEnfermedades finCadena (enfermedades raton)}

eliminarEnfermedades :: String -> [Enfermedad] -> [Enfermedad]
eliminarEnfermedades finCadena enfermedades = filter (ultimasLetrasDistintas finCadena) enfermedades

ultimasLetrasDistintas :: String -> Enfermedad -> Bool
ultimasLetrasDistintas finCadena enfermedad = drop (max 0 (length enfermedad - length finCadena)) enfermedad /= finCadena

alcachofa :: Hierba
alcachofa raton | peso raton > 2 = raton {peso=0.9*peso raton}
                | otherwise = raton {peso= 0.5*peso raton}

hierbaSort :: Hierba
hierbaSort raton = raton {nombre="Pinky",edad = 0, enfermedades=[]}

hierbaDelDiablo :: Hierba
hierbaDelDiablo raton = raton {peso = max 0 (peso raton-0.1),enfermedades = eliminarEnfermedadesCortas (enfermedades raton)}

eliminarEnfermedadesCortas :: [Enfermedad] -> [Enfermedad]
eliminarEnfermedadesCortas enfermedades = filter ((>=10).length) enfermedades

{---------------------------PUNTO 3---------------------------}
type Medicamento = [Hierba]

administrarMedicamento :: Medicamento -> Raton -> Raton
administrarMedicamento medicamento raton = foldl ingerirHierba raton medicamento

pondsAntiAge :: Medicamento
pondsAntiAge = [hierbaBuena,hierbaBuena,hierbaBuena,alcachofa]

reduceFatFast :: Number -> Medicamento
reduceFatFast potencia = [hierbaVerde "Obesidad"] ++ take potencia (repeat alcachofa)

pdepCilina :: Medicamento
pdepCilina = map hierbaVerde sufijosInfecciosas

sufijosInfecciosas = [ "sis", "itis", "emia", "cocos"]

{---------------------------PUNTO 4---------------------------}

cantidadIdeal :: (Number -> Bool) -> Number
cantidadIdeal condicion = primeroQueCumple condicion 0

primeroQueCumple :: (Number->Bool) -> Number -> Number
primeroQueCumple condicion n | condicion n = n
                             | otherwise = primeroQueCumple condicion (n+1)

type Comunidad = [Raton]
{-
lograEstabilizar :: Comunidad -> Medicamento -> Bool
lograEstabilizar comunidad medicamento = foldl estaEstabilizado True (map (administrarMedicamento medicamento) comunidad)

estaEstabilizado :: Bool -> Raton -> Bool
estaEstabilizado bool raton = bool && ((<1).peso) raton && ((<3).length) (enfermedades raton)
-}
lograEstabilizar :: Comunidad -> Medicamento -> Bool
lograEstabilizar (raton:[]) medicamento = estaEstabilizado (administrarMedicamento medicamento raton)
lograEstabilizar (raton:ratones) medicamento | estaEstabilizado (administrarMedicamento medicamento raton) = lograEstabilizar ratones medicamento
                                             | otherwise = False
estaEstabilizado :: Raton -> Bool
estaEstabilizado raton = ((<1).peso) raton && ((<3).length) (enfermedades raton)

ratoncitos :: Comunidad
ratoncitos = [bicenterrata,huesudo]

todosLosRatones :: Comunidad
todosLosRatones = [cerebro,bicenterrata,huesudo]

potenciaIdeal :: Comunidad -> Number
potenciaIdeal comunidad = calcularPotencia comunidad 0

calcularPotencia :: Comunidad -> Number -> Number
calcularPotencia comunidad n | lograEstabilizar comunidad (reduceFatFast n) = n
                             | otherwise = calcularPotencia comunidad (n+1)

{---------------------------PUNTO 5---------------------------}
{-
a-Es imposible saber si un medicamento estabiliza a esta comunidad infinita ya que como todos los ratones cumplirán con
los requisitos nunca terminaría de recorrer a la comunidad.
b-Es posible ya que al encontrar a un raton que no está estabilizado, devolverá false, por lo que no necesita cargar toda la lista.
-}
{---------------------------PUNTO 5---------------------------}
{-
a- Ningun cambio, simplemente se la agrega y se le da el tipo Hierba.
b- El concepto es el de orden superior, y en este caso sirve para que una función reciba como parámetro a otra función y la ejecute.
En este caso, la función ingerirHierba recibe a cualquier tipo de hierba y le asigna sus efectos a un raton.
c- Si se cambia el peso del raton a libras, habría que modificar las funciones alcachofa, hierbaDelDiablo y estaEstabilizado, ya
que son las únicas que utilizan el peso en kilogramos.
-}