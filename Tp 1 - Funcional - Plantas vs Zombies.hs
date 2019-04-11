--Modelado genérico del tipo Planta
data Planta = Planta{
especie :: String,
puntosVida :: Int,
cantSoles :: Int,
poderAtaque :: Int
} deriving (Show)

--peaShooter
peaShooter = Planta {
especie = "peaShooter",
puntosVida = 5,
cantSoles = 0,
poderAtaque = 2
}

--Repeater
repeater = Planta {
especie = "Repeater",
puntosVida = puntosVida peaShooter,
cantSoles = cantSoles peaShooter,
poderAtaque = poderAtaque peaShooter * 2
}

--SunFlower
sunFlower = Planta {
especie = "SunFlower",
puntosVida = 7,
cantSoles = 1,
poderAtaque = 0
}

--Nut
nut = Planta {
especie = "Nut",
puntosVida = 100,
cantSoles = 0,
poderAtaque = 0
}


--Modelado generico del tipo Zombie
data Zombie = Zombie {
nombre :: String,
accesorios :: [String],
daño :: Int,
nivelDeMuerte :: Int
} deriving (Show)

--Zombie Base
zombieBase = Zombie {
nombre = "Zombie",
accesorios = [],
daño = 1,
nivelDeMuerte = (nivelDeMuerteDe.nombre) zombieBase
}

--Balloon Zombie
balloonZombie = Zombie {
nombre = "Pepe Colgado",
accesorios = ["Globo"],
daño = daño zombieBase,
nivelDeMuerte = (nivelDeMuerteDe.nombre) balloonZombie
}

--Newspaper Zombie
newsPaperZombie = Zombie {
nombre = "Beto el chismoso",
accesorios = ["Diario"],
daño = 2,
nivelDeMuerte = (nivelDeMuerteDe.nombre) newsPaperZombie
}

--Gargantuar
gargantuar = Zombie {
nombre = "Gargantuar Hulk Smash Puny God",
accesorios = ["Poste", "Zombie Enano"],
daño = 30,
nivelDeMuerte = (nivelDeMuerteDe.nombre) gargantuar
}

--Determinar Nivel de Muerte
nivelDeMuerteDe :: String -> Int
nivelDeMuerteDe nombre = length nombre

--2--
--a--Defino especialidad
especialidad :: Planta -> String
especialidad planta
 |cantSoles planta > 0 = "Proveedora"
 |poderAtaque planta *2 > puntosVida planta = "Atacante"
 |otherwise = "Defensiva"
 
--b--Defino peligro
esPeligroso :: Zombie -> Bool
esPeligroso zombie = (length.accesorios) zombie > 1 || nivelDeMuerte zombie > 10

--3--
--Defino listas Plantas y Zombies
data LineaDeDefensa = LineaDeDefensa{
plantas :: [Planta],
zombies :: [Zombie]
} deriving (Show)

--Genero las lineas
linea1 = LineaDeDefensa{
plantas = [sunFlower, sunFlower, sunFlower],
zombies = []
}

linea2 = LineaDeDefensa{
plantas = [peaShooter, peaShooter, sunFlower,nut],
zombies = [zombieBase, newsPaperZombie]
}

linea3 = LineaDeDefensa{
plantas = [sunFlower, peaShooter],
zombies = [gargantuar,zombieBase,zombieBase]
}

linea4 = LineaDeDefensa{
plantas = [peaShooter],
zombies = [zombieBase]
}

--3a--
agregarPlantaALinea :: Planta -> LineaDeDefensa -> LineaDeDefensa
agregarPlantaALinea planta linea = LineaDeDefensa{
plantas = plantas linea ++ [planta],
zombies = zombies linea
}

agregarZombieALinea :: Zombie -> LineaDeDefensa -> LineaDeDefensa
agregarZombieALinea zombie linea = LineaDeDefensa{
plantas = plantas linea,
zombies = zombies linea ++ [zombie]
}

--3b--
estaEnPeligro :: LineaDeDefensa -> Bool
estaEnPeligro linea = (poderTotalDeAtaqueDe linea < poderTotalDeMordiscosDe linea )|| (sonTodosZombiesPeligrososDe linea && hayUnZombieEn linea )

poderTotalDeAtaqueDe :: LineaDeDefensa -> Int
poderTotalDeAtaqueDe linea = sum.(map poderAtaque) $ (plantas linea)

poderTotalDeMordiscosDe :: LineaDeDefensa -> Int
poderTotalDeMordiscosDe linea = sum.(map daño) $ (zombies linea)

sonTodosZombiesPeligrososDe :: LineaDeDefensa -> Bool
sonTodosZombiesPeligrososDe linea = all esPeligroso (zombies linea)

hayUnZombieEn :: LineaDeDefensa -> Bool
hayUnZombieEn linea = (>0).length $ (zombies linea)

--3c--
necesitaSerDefendida :: LineaDeDefensa -> Bool
necesitaSerDefendida linea = all ((=="Proveedora").especialidad) (plantas linea)

--3d--
--Qué pasaría al consultar si una línea está en peligro si hubiera una cantidad infinita de zombies bases en la misma?
--Se entiende que el poder total de daño de los Zombies siempre seria mayor al poder de Ataque de las plantas, lo que significaría que la linea estaría 
--en peligro siempre, sin importar el valor de Poder de Ataque de las plantas

--Qué pasaría al consultar si una línea con una cantidad infinita de PeaShooters necesita ser defendida? ¿Y con una cantidad infinita de Sunflowers?
--RTA: Con una cantidad infinita de PeaShooters los puntos de Ataque siempre superarian a los mordiscos, y la linea nunca estaría en peligro.
--RTA2: Con una cantidad infinita de SunFlowers los puntos de Ataque se mantendrian en 0 y la linea estaría en peligro si hay al menos un zombie en la misma.

--4a--
lineaMixta :: LineaDeDefensa -> Bool
lineaMixta linea = esMixta (plantas linea) && lineaTiene2Plantas (plantas linea)

--Verifico si la lista de plantas es mixta
esMixta :: [Planta] -> Bool
esMixta [x] = True
esMixta [] = True
esMixta (x:y:ys)
 |especialidad x == especialidad y = False
 |otherwise = esMixta (y:ys)

--Verifico si la lista tiene al menos 2 plantas
lineaTiene2Plantas :: [Planta] -> Bool
lineaTiene2Plantas (x:y:ys) = True
lineaTiene2Plantas [x] = False
lineaTiene2Plantas [] = False