module Library where
import PdePreludat

data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Number,
  precisionJugador :: Number
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Number,
  precision :: Number,
  altura :: Number
} deriving (Eq, Show)

type Puntos = Number

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)

mayorSegun f a b
  | f a > f b = a
  | otherwise = b

type Palo = Habilidad -> Tiro

putter :: Palo
putter habilidad = UnTiro{velocidad = 10, precision = precisionJugador habilidad * 2, altura = 0}

madera :: Palo
madera habilidad = UnTiro{velocidad = 100, precision = precisionJugador habilidad / 2, altura = 5}

hierro :: Number -> Palo
hierro n habilidad = UnTiro{velocidad = fuerzaJugador habilidad * n, precision = precisionJugador habilidad / n, altura = max 0 (n-3)}

palos :: [Palo]
palos = [putter,madera] ++ map hierro [1..10] 

golpe :: Palo -> Jugador  -> Tiro
golpe palo = palo . habilidad

data Obstaculo = TunelConRampa | Laguna | Hoyo

puedeSuperarlo :: Tiro -> Obstaculo -> Bool

puedeSuperarlo tiro TunelConRampa = precision tiro > 90
puedeSuperarlo tiro Laguna = (...)