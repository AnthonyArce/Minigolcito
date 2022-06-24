module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

-- Modelo inicial
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
tiroUno = UnTiro 100 50 25

data Tiro = UnTiro {
velocidad :: Number,
precision :: Number,
altura :: Number
} deriving (Eq, Show)

type Puntos = Number
type Palo = Habilidad -> Tiro
type Palos = [Palo]
-- type Obstaculo = Tiro -> Tiro
-- Funciones útiles
between n m x = elem x [n .. m]
maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b | f a > f b = a
                 | otherwise = b

---- Funciones auxiliares


----------------------Punto  1
operacionConPrecision::(Number -> Number ) -> Habilidad -> Number
operacionConPrecision f habilidad = f . precisionJugador $ habilidad

putter:: Palo
putter habilidad = UnTiro 10 (operacionConPrecision (*2) habilidad )  0 

madera:: Palo
madera habilidad = UnTiro 100 ( operacionConPrecision (/2) habilidad ) 5 

hierro:: Number -> Palo
hierro n habilidad = (UnTiro ( (*n) . fuerzaJugador $ habilidad )  ( operacionConPrecision (/2) habilidad )  (max 0 (n - 3))) 


palos = [putter, madera] ++ map (hierro) [1..10]

--------------------------Punto 2

golpe::Jugador -> Palo -> Tiro
golpe jugador palo = palo . habilidad $ jugador

-------------------------Punto 3
tiroParado = UnTiro 0 0 0


data Obstaculo = UnObstaculo {
    superaObstaculo:: Tiro -> Bool,
    efectoObstaculo:: Tiro -> Tiro
}deriving Show


superarObstaculo::(Tiro -> Tiro) -> ( Tiro -> Bool ) -> Tiro -> Tiro
superarObstaculo efecto superaObtaculo tiro | superaObtaculo tiro = efecto tiro
                                            | otherwise = tiroParado


-- tunelConRampita:: Obstaculo  //habilitar type obstaculo = Tiro -> Tiro
-- tunelConRampita  = superarObstaculo efectoTunelConRampita superaTunelConRampita

tunelConRampita:: Obstaculo
tunelConRampita = UnObstaculo superaTunelConRampita efectoTunelConRampita

efectoTunelConRampita::Tiro -> Tiro
efectoTunelConRampita unTiro = unTiro { velocidad =( (*2) . velocidad $ unTiro), precision = 100  } 

superaTunelConRampita::Tiro -> Bool 
superaTunelConRampita unTiro = (precision unTiro > 90 ) && (altura unTiro == 0)



-- laguna::Number -> Obstaculo
-- laguna largoLaguna = superarObstaculo (efectoLaguna largoLaguna) superaLaguna

laguna::Number -> Obstaculo
laguna largoLaguna = UnObstaculo superaLaguna (efectoLaguna largoLaguna)

efectoLaguna::Number -> Tiro -> Tiro
efectoLaguna largoLaguna unTiro = unTiro { altura = (altura unTiro) / largoLaguna }

superaLaguna::Tiro -> Bool
superaLaguna unTiro = ( (> 80) . velocidad $ unTiro ) && ( (between 1 5) . altura $ unTiro)   



-- hoyo::Obstaculo
-- hoyo  = superarObstaculo efectoHoyo superaHoyo

hoyo::Obstaculo
hoyo = UnObstaculo superaHoyo efectoHoyo

efectoHoyo::Tiro -> Tiro
efectoHoyo _ = tiroParado

superaHoyo::Tiro -> Bool
superaHoyo unTiro = ( (between 5 20) . velocidad $ unTiro ) && ( (==0) . altura $ unTiro )  && ( (>95) . precision $ unTiro )



------------------------Punto 4

----------- A
paloUtil:: Jugador -> Obstaculo -> Palo -> Bool
paloUtil jugador obstaculo unPalo = (superaObstaculo obstaculo) . unPalo . habilidad $ jugador

palosUtiles:: Jugador -> Obstaculo -> [Palo]
palosUtiles unJugador unObstaculo = filter ( paloUtil unJugador unObstaculo ) palos


---------- B

funcion::Tiro -> Obstaculo -> Bool
funcion unTiro unObstaculo = superaObstaculo unObstaculo unTiro

funcionDos::Tiro -> [Obstaculo] -> Tiro
funcionDos unTiro obstaculos = foldl (\tiro unObstaculo ->  efectoObstaculo unObstaculo tiro) unTiro obstaculos
-- superaObstaculo Obstaculo tiro

-- cuantoObstaculoSuperaConsecutivamente::[Obstaculo] -> Tiro -> Number
-- cuantoObstaculoSuperaConsecutivamente obstaculos tiro = length (takeWhile ( (\unTiro unObstaculo -> superaObstaculo unObstaculo unTiro) tiro )  obstaculos)


cuantoObstaculoSuperaConsecutivamente::[Obstaculo] -> Tiro -> Number
cuantoObstaculoSuperaConsecutivamente [] _ = 0
cuantoObstaculoSuperaConsecutivamente (obstaculo:obstaculos) tiro | superaObstaculo obstaculo tiro = 1 + cuantoObstaculoSuperaConsecutivamente obstaculos (efectoObstaculo obstaculo tiro)
                                                                   | otherwise = 0

----------------- c


palosConMasObstaculosPasdos::[Obstaculo] -> Jugador -> Palo -> Palo -> Palo
palosConMasObstaculosPasdos obstaculos unJugador unPalo otroPalo | ( cuantoObstaculoSuperaConsecutivamente obstaculos ( golpe unJugador unPalo) ) > cuantoObstaculoSuperaConsecutivamente obstaculos (golpe unJugador otroPalo) = unPalo
                                                                 | otherwise = otroPalo



palosMasUtil:: Jugador -> [Obstaculo] -> [Palo] -> Palo
palosMasUtil unJugador obstaculos (palo:palos)= foldl (palosConMasObstaculosPasdos obstaculos unJugador) palo palos 


palosMasUtil':: Jugador -> [Obstaculo] -> Palo
palosMasUtil' unJugador obstaculos = maximoSegun ( cuantoObstaculoSuperaConsecutivamente obstaculos . golpe unJugador ) palos

-------------- 5

jugadorTorneo = fst
puntosGanados = snd

padresPerdedores:: [ ( Jugador, Puntos) ] -> [String]
padresPerdedores lista = (map ( padre.jugadorTorneo ) ) . ( filter ( not.gano lista ) ) $ lista


gano :: [(Jugador, Puntos)] -> (Jugador, Puntos) -> Bool
gano puntosDeTorneo puntosDeUnJugador = (all ( (< puntosGanados puntosDeUnJugador).puntosGanados ) . filter (/= puntosDeUnJugador)) puntosDeTorneo